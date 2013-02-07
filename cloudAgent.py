import hashlib
import os

import dmp
import dmp_monkey
import utils

dmp_monkey.monkey_patch()
DMP = dmp.diff_match_patch()


class CloudAgent():
    CLOUD_VERSION = "0.01"

    def sendToCloud(self, req):
        raise NotImplemented('')

    def cloud_room_info(self, req):
        self.perms = req['perms']
        self.tree = req['tree']
        self.projectPath = utils.unfuck_path(os.path.abspath('./shared'))
        self.sendToEditor(req)
        self.readOnly = True

        if 'patch' not in self.perms:
            print("We don't have patch permission. Setting buffers to read-only")
            self.readOnly = True

        self.basePath = os.path.join(self.projectPath, self.room_owner, self.room)
        print('making dir', self.basePath)
        utils.mkdir(self.basePath)

        self.bufs = {}
        for bufId, buf in req['bufs'].iteritems():
            bufId = int(bufId)  # json keys must be strings
            self.bufs[bufId] = buf
            newDir = os.path.split(os.path.join(self.basePath, buf['path']))[0]
            utils.mkdir(newDir)
            self.sendToCloud({
                'name': 'get_buf',
                'id': bufId,
            })

        print('Successfully joined room %s/%s' % (self.room_owner, self.room))

    def cloud_patch(self, req):
        bufId = req['id']
        buf = self.bufs[bufId]
        patches = DMP.patch_fromText(req['patch'])

        if buf['md5'] != req['md5_before']:
            print('starting md5s don\'t match for %s. this is dangerous!' % buf['path'])

        t = DMP.patch_apply(patches, buf['buf'])

        clean_patch = True
        for applied_patch in t[1]:
            if not applied_patch:
                clean_patch = False
                break
        if not clean_patch:
            print('Patch not applied cleanly. Re-fetching buffer')
            self.sendToCloud({
                'name': 'get_buf',
                'id': bufId,
            })
            return

        md5_after = hashlib.md5(t[0].encode('utf-8')).hexdigest()
        if md5_after != req['md5_after']:
            print('%s new hash %s != expected %s. re-fetching buffer...' % \
                (buf['path'], md5_after, req['md5_after']))

        buf['buf'] = t[0]
        buf['md5'] = md5_after

        req['offsets'] = t[2]
        req['buf'] = buf
        self.sendToEditor({
            'name': 'edit',
            'edits': t[2],
            'full_path': buf['full_path'],
        })

            # new_sels = []
            # for sel in selections:
            #     a = sel.a
            #     b = sel.b
            #     new_offset = len(patch_text) - length
            #     if sel.a > offset:
            #         a += new_offset
            #     if sel.b > offset:
            #         b += new_offset
            #     new_sels.append(sublime.Region(a, b))

    def cloud_get_buf(self, req):
        bufId = req['id']
        self.bufs[bufId] = req
        filePath = os.path.join(self.basePath, req['path'])
        fileDir = os.path.split(filePath)[0]
        utils.mkdir(fileDir)
        with open(filePath, 'w') as fd:
            fd.write(req['buf'])
        self.bufs[bufId]['full_path'] = filePath
        print('got buf %s. buf is now %s' % (bufId, self.bufs[bufId]))
        req['full_path'] = filePath
        self.sendToEditor(req)
        del self.bufs[bufId]['name']

    def cloud_create_buf(self, req):
        self.cloud_get_buf(req)

    def cloud_rename_buf(self, req):
        pass

    def cloud_delete_buf(self, req):
        pass

    def cloud_join(self, req):
        self.sendToEditor(req)

    def cloud_part(self, req):
        self.sendToEditor(req)

    def cloud_highlight(self, req):
        pass

    def cloud_error(self, req):
        self.sendToEditor(req)

    def cloud_disconnect(self, req):
        self.cloudFactory.reconnect = False
        self.sendToEditor(req)
        print('diconnected: ', req)

    def cloud_msg(self):
        pass
