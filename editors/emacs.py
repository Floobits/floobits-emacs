import hashlib
from twisted.internet import reactor

import dmp
import dmp_monkey
import cloudFactory

dmp_monkey.monkey_patch()
DMP = dmp.diff_match_patch()


class Emacs():
    EDITOR_VERSION = "0.01"

    def editor_auth(self, req):
        print req
        self.room = req['room']
        self.username = req['username']
        self.room_owner = req.get('room_owner', self.username)
        self.secret = req['secret']
        self.cloudFactory = cloudFactory.CloudFactory(self)
        # TODO: make these a setting. also, ssl
        reactor.connectTCP("floobits.com", 3148, self.cloudFactory)

    def editor_change(self, req):
        path = req['full_path']
        if not path:
            return

        buf = self.getBufByPath(path)
        if not buf:
            print("buf not found for path %s. not sending patch" % path)
            return

        patches = DMP.patch_make(buf['buf'], req['after'])
        patch_str = ''
        for patch in patches:
            patch_str += str(patch)

        md5_before = buf['md5']
        buf['md5'] = hashlib.md5(req['after'].encode('utf-8')).hexdigest()
        self.sendToCloud({
            'name': 'patch',
            'id': buf['id'],
            'md5_after': buf['md5'],
            'md5_before': md5_before,
            'patch': patch_str,
        })
        buf['buf'] = req['after']
        print buf

    def editor_buffer_list(self, req):
        print req
