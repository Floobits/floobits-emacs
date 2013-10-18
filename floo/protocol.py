"""Understands the floobits protocol"""

import os
import sys
import hashlib
import collections
import Queue
import base64

from common import msg, ignore, shared as G, utils
from common.lib import diff_match_patch as dmp

import sublime


DMP = dmp.diff_match_patch()


def buf_populated(func):
    def wrapped(self, data):
        if data.get('id') is None:
            msg.debug('no buf id in data')
            return
        buf = self.FLOO_BUFS.get(data['id'])
        if buf is None or 'buf' not in buf:
            msg.debug('buf is not populated yet')
            return
        func(self, data)
    return wrapped


class BaseProtocol(object):
    BUFS_CHANGED = []
    SELECTION_CHANGED = []
    MODIFIED_EVENTS = Queue.Queue()
    SELECTED_EVENTS = Queue.Queue()
    FLOO_BUFS = {}
    FLOO_PATHS_TO_BUFS = {}

    def __init__(self, agent):
        self.agent = agent
        self.perms = []
        self.follow_mode = False
        self.chat_deck = collections.deque(maxlen=10)
        self.ignored_names = ['node_modules']

    def get_view(self, data):
        raise NotImplemented()

    def update_view(self, data, view):
        raise NotImplemented()

    def get_buf(self, data):
        raise NotImplemented()

    def get_buf_by_path(self, path):
        rel_path = utils.to_rel_path(path)
        buf_id = self.FLOO_PATHS_TO_BUFS.get(rel_path)
        if buf_id:
            return self.FLOO_BUFS.get(buf_id)

    def save_buf(self, buf):
        path = utils.get_full_path(buf['path'])
        utils.mkdir(os.path.split(path)[0])
        with open(path, 'wb') as fd:
            if buf['encoding'] == 'utf8':
                fd.write(buf['buf'].encode('utf-8'))
            else:
                fd.write(buf['buf'])
        return path

    def chat(self, data):
        raise NotImplemented()

    def maybe_buffer_changed(self):
        raise NotImplemented()

    def maybe_selection_changed(self):
        raise NotImplemented()

    def on_msg(self, data):
        raise NotImplemented()

    def is_shared(self, p):
        if not self.agent.is_ready():
            msg.debug('agent is not ready. %s is not shared' % p)
            return False
        p = utils.unfuck_path(p)
        # TODO: tokenize on path seps and then look for ..
        if utils.to_rel_path(p).find("../") == 0:
            return False
        return True

    def follow(self, follow_mode=None):
        if follow_mode is None:
            follow_mode = not self.follow_mode
        self.follow_mode = follow_mode
        msg.log('follow mode is %s' % {True: 'enabled', False: 'disabled'}[self.follow_mode])

    def create_buf(self, path, force=False):
        if not utils.is_shared(path):
            msg.error('Skipping adding %s because it is not in shared path %s' % (path, G.PROJECT_PATH))
            return
        ig = ignore.Ignore(None, path)
        self._uploader(ig.list_paths())

    def _uploader(self, paths_iter):
        if not self.agent or not self.agent.sock:
            msg.error('Can\'t upload! Not connected. :(')
            return

        self.agent.tick()
        if self.agent.qsize() > 0:
            return utils.set_timeout(self._uploader, 10, paths_iter)
        try:
            p = next(paths_iter)
            self.upload(p)
        except StopIteration:
            msg.log('All done uploading')
            return
        return utils.set_timeout(self._uploader, 50, paths_iter)

    def upload(self, path):
        try:
            with open(path, 'rb') as buf_fd:
                buf = buf_fd.read()
            encoding = 'utf8'
            rel_path = utils.to_rel_path(path)
            existing_buf = self.get_buf_by_path(path)
            if existing_buf:
                buf_md5 = hashlib.md5(buf).hexdigest()
                if existing_buf['md5'] == buf_md5:
                    msg.debug('%s already exists and has the same md5. Skipping.' % path)
                    return
                msg.log('setting buffer ', rel_path)

                try:
                    buf = buf.decode('utf-8')
                except Exception:
                    buf = base64.b64encode(buf).decode('utf-8')
                    encoding = 'base64'

                existing_buf['buf'] = buf
                existing_buf['encoding'] = encoding
                existing_buf['md5'] = buf_md5

                self.agent.put({
                    'name': 'set_buf',
                    'id': existing_buf['id'],
                    'buf': buf,
                    'md5': buf_md5,
                    'encoding': encoding,
                })
                return

            try:
                buf = buf.decode('utf-8')
            except Exception:
                buf = base64.b64encode(buf).decode('utf-8')
                encoding = 'base64'

            msg.log('creating buffer ', rel_path)
            event = {
                'name': 'create_buf',
                'buf': buf,
                'path': rel_path,
                'encoding': encoding,
            }
            self.agent.put(event)
        except (IOError, OSError):
            msg.error('Failed to open %s.' % path)
        except Exception as e:
            msg.error('Failed to create buffer %s: %s' % (path, unicode(e)))

    def handle(self, data):
        name = data.get('name')
        if not name:
            return msg.error('no name in data?!?')
        func = getattr(self, "on_%s" % (name), None)
        if not func:
            return msg.debug('unknown name', name, 'data:', data)
        try:
            func(data)
        except Exception as e:
            msg.error(str(e))

    def push(self):
        reported = set()
        while self.BUFS_CHANGED:
            buf_id = self.BUFS_CHANGED.pop()
            view = self.get_view(buf_id)
            buf = view.buf
            if view.is_loading():
                msg.debug('View for buf %s is not ready. Ignoring change event' % buf['id'])
                continue
            if 'patch' not in self.perms:
                continue
            vb_id = view.native_id
            if vb_id in reported:
                continue
            if 'buf' not in buf:
                msg.debug('No data for buf %s %s yet. Skipping sending patch' % (buf['id'], buf['path']))
                continue

            reported.add(vb_id)
            patch = utils.FlooPatch(view.get_text(), view.buf)
            # Update the current copy of the buffer
            buf['buf'] = patch.current
            buf['md5'] = hashlib.md5(patch.current.encode('utf-8')).hexdigest()
            self.agent.put(patch.to_json())

        while self.SELECTION_CHANGED:
            view, ping = self.SELECTION_CHANGED.pop()
            # consume highlight events to avoid leak
            if 'highlight' not in self.perms:
                continue
            vb_id = view.native_id
            if vb_id in reported:
                continue

            reported.add(vb_id)
            highlight_json = {
                'id': view.buf['id'],
                'name': 'highlight',
                'ranges': view.get_selections(),
                'ping': ping,
            }
            self.agent.put(highlight_json)

    def on_create_buf(self, data):
        self.on_get_buf(data)

    def on_get_buf(self, data):
        buf_id = data['id']
        if data['encoding'] == 'base64':
            data['buf'] = base64.b64decode(data['buf'])
        self.FLOO_BUFS[buf_id] = data
        self.FLOO_PATHS_TO_BUFS[data['path']] = buf_id
        view = self.get_view(buf_id)
        if view:
            self.update_view(data, view)
        else:
            self.save_buf(data)

    def rename_buf(self, buf_id, new_path):
        new_path = utils.to_rel_path(new_path)
        if not utils.is_shared(new_path):
            msg.log('New path %s is not shared. Discarding rename event.' % new_path)
            return
        self.agent.put({
            'name': 'rename_buf',
            'id': buf_id,
            'path': new_path,
        })
        old_path = self.FLOO_BUFS[buf_id]['path']
        del self.FLOO_PATHS_TO_BUFS[old_path]
        self.FLOO_PATHS_TO_BUFS[new_path] = buf_id
        self.FLOO_BUFS[buf_id]['path'] = new_path

    def on_rename_buf(self, data):
        buf_id = int(data['id'])
        new = utils.get_full_path(data['path'])
        old = utils.get_full_path(data['old_path'])
        new_dir = os.path.dirname(new)
        if new_dir:
            utils.mkdir(new_dir)
        buf = self.FLOO_BUFS[buf_id]
        old_path = buf['path']
        del self.FLOO_PATHS_TO_BUFS[old_path]
        self.FLOO_PATHS_TO_BUFS[new] = buf_id
        self.FLOO_BUFS[buf_id]['path'] = new

        view = self.get_view(buf_id)
        if view:
            view.rename(new)
        else:
            os.rename(old, new)

    def on_room_info(self, data):
        # Success! Reset counter
        self.workspace_info = data
        self.perms = data['perms']

        if 'patch' not in data['perms']:
            msg.log('We don\'t have patch permission. Setting buffers to read-only')

        utils.mkdir(G.PROJECT_PATH)
        msg.debug('mkdir -p for project path %s' % G.PROJECT_PATH)

        floo_json = {
            'url': utils.to_workspace_url({
                'host': self.agent.host,
                'owner': self.agent.owner,
                'port': self.agent.port,
                'workspace': self.agent.workspace,
                'secure': self.agent.secure,
            })
        }
        utils.update_floo_file(os.path.join(G.PROJECT_PATH, '.floo'), floo_json)

        G.JOINED_WORKSPACE = True

        bufs_to_get = []
        new_dirs = set()
        for buf_id, buf in data['bufs'].items():
            buf_id = int(buf_id)  # json keys must be strings
            buf_path = utils.get_full_path(buf['path'])
            new_dir = os.path.dirname(buf_path)
            if new_dir not in new_dirs:
                utils.mkdir(new_dir)
                new_dirs.add(new_dir)
            self.FLOO_BUFS[buf_id] = buf
            self.FLOO_PATHS_TO_BUFS[buf['path']] = buf_id

            try:
                buf_fd = open(buf_path, 'rb')
            except Exception as e:
                msg.debug("Couldn't read %s: %s" % (buf_path, e))
                bufs_to_get.append(buf_id)
                continue
            else:
                buf_buf = buf_fd.read()
            md5 = hashlib.md5(buf_buf).hexdigest()
            if md5 == buf['md5']:
                msg.debug('md5 sums match- not fetching buffer %s' % buf_path)
                if buf['encoding'] == 'utf8':
                    buf['buf'] = buf_buf.decode('utf-8')
                continue
            bufs_to_get.append(buf_id)

        def finish_room_info():
            success_msg = 'Successfully joined workspace %s/%s' % (self.agent.owner, self.agent.workspace)
            msg.log(success_msg)
            self.agent.on_auth()

        if bufs_to_get and self.agent.get_bufs:
            if len(bufs_to_get) > 4:
                prompt = '%s local files are different from the workspace. Overwrite your local files?' % len(bufs_to_get)
            else:
                prompt = 'Overwrite the following local files: %s ? ' % " ".join([self.FLOO_BUFS[buf_id]['path']
                         for buf_id in bufs_to_get])

            def stomp_local(data):
                d = data['response']
                for buf_id in bufs_to_get:
                    if d:
                        self.agent.send_get_buf(buf_id)
                    else:
                        buf = self.FLOO_BUFS[buf_id]
                        # TODO: this is inefficient. we just read the file 20 lines ago
                        self.create_buf(utils.get_full_path(buf['path']))
                finish_room_info()
            self.agent.conn.get_input(prompt, '', stomp_local, y_or_n=True)
        else:
            finish_room_info()

    def on_join(self, data):
        msg.log('%s joined the workspace' % data['username'])

    def on_part(self, data):
        msg.log('%s left the workspace' % data['username'])
        region_key = 'floobits-highlight-%s' % (data['user_id'])
        for window in sublime.windows():
            for view in window.views():
                view.erase_regions(region_key)

    @buf_populated
    def on_patch(self, data):
        if len(data['patch']) == 0:
            msg.error('wtf? no patches to apply. server is being stupid')
            return

        buf_id = data['id']
        buf = self.FLOO_BUFS[buf_id]
        if buf['encoding'] == 'base64':
            # TODO apply binary patches
            return self.agent.send_get_buf(buf_id)

        view = self.get_view(buf_id)

        dmp_patches = DMP.patch_fromText(data['patch'])
        # TODO: run this in a separate thread
        if view:
            old_text = view.get_text()
        else:
            old_text = buf.get('buf', '')
        md5_before = hashlib.md5(old_text.encode('utf-8')).hexdigest()
        if md5_before != data['md5_before']:
            msg.debug('maybe vim is lame and discarded a trailing newline')
            old_text += '\n'
        md5_before = hashlib.md5(old_text.encode('utf-8')).hexdigest()
        if md5_before != data['md5_before']:
            msg.warn('starting md5s don\'t match for %s. ours: %s patch: %s this is dangerous!' %
                    (buf['path'], md5_before, data['md5_before']))

        t = DMP.patch_apply(dmp_patches, old_text)

        clean_patch = True
        for applied_patch in t[1]:
            if not applied_patch:
                clean_patch = False
                break

        if G.DEBUG:
            if len(t[0]) == 0:
                msg.debug('OMG EMPTY!')
                msg.debug('Starting data:', buf['buf'])
                msg.debug('Patch:', data['patch'])
            if '\x01' in t[0]:
                msg.debug('FOUND CRAZY BYTE IN BUFFER')
                msg.debug('Starting data:', buf['buf'])
                msg.debug('Patch:', data['patch'])

        if not clean_patch:
            msg.error('failed to patch %s cleanly. re-fetching buffer' % buf['path'])
            return self.agent.send_get_buf(buf_id)

        cur_hash = hashlib.md5(t[0].encode('utf-8')).hexdigest()
        if cur_hash != data['md5_after']:
            msg.warn(
                '%s new hash %s != expected %s. re-fetching buffer...' %
                (buf['path'], cur_hash, data['md5_after'])
            )
            return self.agent.send_get_buf(buf_id)

        buf['buf'] = t[0]
        buf['md5'] = cur_hash

        if not view:
            self.save_buf(buf)
            return
        view.apply_patches(buf, t)

    def delete_buf(self, path):
        """deletes a path"""

        if not path:
            return

        path = utils.get_full_path(path)

        if not self.is_shared(path):
            msg.error('Skipping deleting %s because it is not in shared path %s.' % (path, G.PROJECT_PATH))
            return

        if os.path.isdir(path):
            for dirpath, dirnames, filenames in os.walk(path):
                # Don't care about hidden stuff
                dirnames[:] = [d for d in dirnames if d[0] != '.']
                for f in filenames:
                    f_path = os.path.join(dirpath, f)
                    if f[0] == '.':
                        msg.log('Not deleting buf for hidden file %s' % f_path)
                    else:
                        self.delete_buf(f_path)
            return
        buf_to_delete = None
        rel_path = utils.to_rel_path(path)
        buf_to_delete = self.get_buf_by_path(rel_path)
        if buf_to_delete is None:
            msg.error('%s is not in this workspace' % path)
            return
        msg.log('deleting buffer ', rel_path)
        event = {
            'name': 'delete_buf',
            'id': buf_to_delete['id'],
        }
        self.agent.put(event)

    @buf_populated
    def on_delete_buf(self, data):
        buf_id = int(data['id'])
        del self.FLOO_BUFS[buf_id]
        del self.FLOO_PATHS_TO_BUFS[data['path']]
        path = utils.get_full_path(data['path'])
        utils.rm(path)
        msg.warn('deleted %s because %s told me too.' % (path, data.get('username', 'the internet')))

    @buf_populated
    def on_highlight(self, data):
        ping = data.get('ping', False)
        if self.follow_mode:
            ping = True
        buf = self.FLOO_BUFS[data['id']]
        view = self.get_view(data['id'])
        if not view:
            if not ping:
                return
            view = self.create_view(buf)
            if not view:
                return
        if ping:
            try:
                offset = data['ranges'][0][0]
            except IndexError as e:
                msg.debug('could not get offset from range %s' % e)
            else:
                msg.log('You have been summoned by %s' % (data.get('username', 'an unknown user')))
                view.focus(offset)
        view.highlight(data['ranges'], data['user_id'], data.get('username', 'unknown user'))

    def on_saved(self, data):
        msg.debug('%s saved %s' % (data.get('username'), data.get('id')))

    def on_error(self, data):
        message = 'Floobits: Error! Message: %s' % str(data.get('msg'))
        if data.get('flash'):
            msg.error(message)
        else:
            msg.log(message)

    def on_disconnect(self, data):
        message = 'Floobits: Disconnected! Reason: %s' % str(data.get('reason'))
        msg.error(message)
        try:
            self.agent.stop()
        except Exception:
            pass
        sys.exit(0)
