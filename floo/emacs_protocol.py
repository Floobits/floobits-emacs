import webbrowser
from collections import defaultdict

from common import msg
from common import shared as G
from common import utils

import protocol

emacs = None


class View(object):
    """editors representation of the buffer"""

    def __init__(self, buf, emacs_buf=None):
        self.buf = buf
        self._emacs_buf = emacs_buf
        if emacs_buf is None:
            emacs.put('create_view', {
                'full_path': utils.get_full_path(buf['path']),
                'id': buf['id'],
            })

    def __repr__(self):
        return '%s %s %s' % (self.native_id, self.buf['id'], self.buf['path'])

    def __str__(self):
        return repr(self)

    @property
    def emacs_buf(self):
        return self._emacs_buf[0]

    @emacs_buf.setter
    def emacs_buf(self, value):
        self._emacs_buf[0] = value

    @property
    def native_id(self):
        return self.buf['id']

    def is_loading(self):
        return self._emacs_buf is None

    def get_text(self):
        return self.emacs_buf

    def set_text(self, text):
        self.emacs_buf = text
        emacs.put('get_buf', {
            'id': self.buf['id'],
            'full_path': utils.get_full_path(self.buf['path']),
            'buf': text,
        })

    def apply_patches(self, buf, patches):
        cursor_offset = self.get_cursor_offset()
        msg.debug('cursor offset is %s bytes' % cursor_offset)

        self.emacs_buf = patches[0]
        emacs.put('edit', {
            'id': self.buf['id'],
            'full_path': utils.get_full_path(self.buf['path']),
            'edits': patches[2],
        })

        for patch in patches[2]:
            offset = patch[0]
            length = patch[1]
            patch_text = patch[2]
            if cursor_offset > offset:
                new_offset = len(patch_text) - length
                cursor_offset += new_offset

        self.set_cursor_position(cursor_offset)

    def focus(self, offset):
        emacs.put('focus', {
            'id': self.buf['id'],
            'full_path': utils.get_full_path(self.buf['path']),
            'offset': offset,
        })

    def set_cursor_position(self, offset):
        pass

    def get_cursor_position(self):
        pass

    def get_cursor_offset(self):
        pass

    def get_selections(self):
        return [[0, 0]]

    def clear_selections(self):
        msg.debug('clearing selections for view %s' % self.buf['path'])

    def highlight(self, ranges, user_id, username):
        msg.debug('highlighting ranges %s' % (ranges))

    def rename(self, name):
        pass

    def save(self):
        pass


class Protocol(protocol.BaseProtocol):
    CLIENT = 'Emacs'

    def __init__(self, *args, **kwargs):
        global emacs
        super(Protocol, self).__init__(*args, **kwargs)
        emacs = G.EMACS
        self.views = {}
        self.emacs_bufs = defaultdict(lambda: [""])

    def create_view(self, buf, emacs_buf=None):
        view = View(buf, emacs_buf)
        self.views[buf['id']] = view
        return view

    def get_view(self, buf_id):
        """Warning: side effects!"""
        view = self.views.get(buf_id)
        if view:
            return view
        buf = self.FLOO_BUFS[buf_id]
        full_path = utils.get_full_path(buf['path'])
        emacs_buf = self.emacs_bufs.get(full_path)
        if emacs_buf:
            view = self.create_view(buf, emacs_buf)
        return view

    def get_view_by_path(self, path):
        """Warning: side effects!"""

        if not path:
            return None
        buf = self.get_buf_by_path(path)
        if not buf:
            msg.debug("buf not found for path %s" % path)
            return None
        view = self.get_view(buf['id'])
        if not view:
            msg.debug("view not found for %s %s" % (buf['id'], buf['path']))
            return None
        return view

    def update_view(self, data, view):
        view.set_text(data['buf'])

    def emacs_handle(self, data):
        name = data.get('name')
        if not name:
            return msg.error('no name in data?!?')
        func = getattr(self, "on_emacs_%s" % (name))
        if not func:
            return msg.debug('unknown name: ', name, 'data: ', data)
        try:
            func(data)
        except Exception as e:
            msg.error(str(e))

    def on_emacs_set_follow_mode(self, req):
        self.follow(bool(req['follow_mode']))

    def on_emacs_change(self, req):
        path = req['full_path']
        view = self.get_view_by_path(path)
        if not view:
            return
        changed = req['changed']
        begin = req['begin']
        old_length = req['old_length']
        self.emacs_bufs[path][0] = "%s%s%s" % (self.emacs_bufs[path][0][:begin - 1], changed, self.emacs_bufs[path][0][begin - 1 + old_length:])
        self.BUFS_CHANGED.append(view.buf['id'])

    def on_emacs_highlight(self, req):
        view = self.get_view_by_path(req['full_path'])
        if view:
            self.SELECTION_CHANGED.append((view, req.get('ping', False)))

    def on_emacs_create_buf(self, req):
        self.create_buf(req['full_path'])

    def on_emacs_delete_buf(self, req):
        buf = self.get_buf_by_path(req['path'])
        if not buf:
            msg.debug('No buffer for path %s' % req['path'])
            return
        msg.log('deleting buffer ', buf['path'])
        event = {
            'name': 'delete_buf',
            'id': buf['id'],
        }
        self.agent.put(event)

    def on_emacs_rename_buf(self, req):
        buf = self.get_buf_by_path(req['old_path'])
        if not buf:
            msg.debug('No buffer for path %s' % req['old_path'])
            return
        self.rename_buf(buf['id'], req['path'])

    def on_emacs_buffer_list_change(self, req):
        added = req.get('added') or {}
        for path, text in added.iteritems():
            buf = self.get_buf_by_path(path)
            self.emacs_bufs[path][0] = text
            if not buf:
                msg.debug('no buf for path %s' % path)
                self.create_buf(path, text=text)
                continue
            view = self.views.get(buf['id'])
            if view is None:
                self.get_view(buf['id'])
            elif view.is_loading():
                view._emacs_buf = self.emacs_bufs[path]
            else:
                msg.debug('view for buf %s already exists. this is not good. we got out of sync' % buf['path'])

        deleted = req.get('deleted') or []
        for path in deleted:
            if self.emacs_bufs.get(path) is None:
                msg.debug('emacs deleted %s but we already deleted it from emacs_bufs' % path)
            del self.emacs_bufs[path]
            buf = self.get_buf_by_path(path)
            if buf:
                del self.views[buf['id']]

        seen = set()
        current = req.get('current') or []
        for path in current:
            if self.emacs_bufs.get(path) is None:
                msg.debug('We should have buffer %s in emacs_bufs but we don\'t' % path)
            else:
                seen.add(path)

        for buf_id, view in self.views.iteritems():
            if utils.get_full_path(view.buf['path']) not in seen:
                msg.debug('We should not have buffer %s in our views but we do.' % view.buf['path'])

    def on_emacs_saved(self, req):
        buf = self.get_buf_by_path(req['path'])
        if not buf:
            msg.debug('No buffer for path %s' % req['path'])
            return
        event = {
            'name': 'saved',
            'id': buf['id'],
        }
        self.agent.put(event)

    def on_emacs_open_workspace(self, req):
        try:
            webbrowser.open(self.agent.workspace_url, new=2, autoraise=True)
        except Exception as e:
            msg.error("Couldn't open a browser: %s" % (str(e)))

    def on_emacs_open_workspace_settings(self, req):
        try:
            webbrowser.open(self.agent.workspace_url + '/settings', new=2, autoraise=True)
        except Exception as e:
            msg.error("Couldn't open a browser: %s" % (str(e)))

    def on_room_info(self, workspace_info):
        super(Protocol, self).on_room_info(workspace_info)
        workspace_info['project_path'] = G.PROJECT_PATH
        emacs.put('room_info', {
            'perms': self.workspace_info['perms'],
            'project_path': G.PROJECT_PATH,
            'workspace_name': self.workspace_info['room_name'],
        })

    def on_create_buf(self, data):
        super(Protocol, self).on_create_buf(data)
        emacs.put('create_buf', {
            'full_path': utils.get_full_path(data['path']),
            'path': data['path'],
            'username': data.get('username', ''),
        })

    def on_delete_buf(self, data):
        buf_id = int(data['id'])
        buf = self.FLOO_BUFS[buf_id]
        path = buf['path']
        try:
            super(Protocol, self).on_delete_buf(data)
        except Exception as e:
            msg.debug('Unable to delete buf %s: %s' % (path, str(e)))
        else:
            emacs.put('delete_buf', {
                'full_path': utils.get_full_path(path),
                'path': path,
                'username': data.get('username', ''),
            })

    def on_rename_buf(self, data):
        buf = self.FLOO_BUFS[int(data['id'])]
        # This can screw up if someone else renames the buffer around the same time as us. Oh well.
        buf = self.get_buf_by_path(utils.get_full_path(data['path']))
        if not buf:
            return super(Protocol, self).on_rename_buf(data)
        msg.debug('We already renamed %s. Skipping' % buf['path'])

    def on_highlight(self, data):
        super(Protocol, self).on_highlight(data)
        buf = self.FLOO_BUFS[data['id']]
        # TODO: save highlights for when user opens the buffer in emacs
        emacs.put('highlight', {
            'id': buf['id'],
            'full_path': utils.get_full_path(buf['path']),
            'ranges': data['ranges'],
            'user_id': data['user_id'],
            'username': data.get('username', 'unknown user'),
        })

    def on_msg(self, data):
        msg.log('msg')
