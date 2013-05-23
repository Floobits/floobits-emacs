from collections import defaultdict

import msg
import protocol
import shared as G
import utils

from lib import diff_match_patch as dmp
import dmp_monkey


dmp_monkey.monkey_patch()
DMP = dmp.diff_match_patch()

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

    def focus(self):
        pass

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

    def highlight(self, ranges, user_id):
        msg.debug('highlighting ranges %s' % (ranges))
        emacs.put('highlight', {
            'id': self.buf['id'],
            'full_path': utils.get_full_path(self.buf['path']),
            'ranges': ranges,
            'user_id': user_id,
        })

    def rename(self, name):
        text = self.buf['buf']
        old_name = self.buf['path']
        with open(name, 'wb') as fd:
            fd.write(text.encode('utf-8'))
        try:
            utils.rm(old_name)
        except Exception as e:
            msg.debug("couldn't delete %s... maybe thats OK?" % str(e))

    def save(self):
        pass


class Protocol(protocol.BaseProtocol):
    CLIENT = 'Emacs'

    def __init__(self, *args, **kwargs):
        global emacs
        super(Protocol, self).__init__(*args, **kwargs)
        # This is intentional. Emacs stores follow_mode state and discards highlights if it's not following
        self.follow_mode = True
        emacs = G.EMACS
        self.views = {}
        self.emacs_bufs = defaultdict(lambda: [""])

    def create_view(self, buf, emacs_buf=None):
        view = View(buf, emacs_buf)
        self.views[buf['id']] = view
        return view

    def get_view(self, buf_id):
        view = self.views.get(buf_id)
        if view:
            return view
        buf = self.FLOO_BUFS[buf_id]
        full_path = utils.get_full_path(buf['path'])
        emacs_buf = self.emacs_bufs.get(full_path)
        if emacs_buf:
            view = self.create_view(buf, emacs_buf)
        return view

    def update_view(self, data, view):
        view.set_text(data['buf'])

    def emacs_handle(self, data):
        msg.debug(data)
        name = data.get('name')
        if not name:
            return msg.error('no name in data?!?')
        func = getattr(self, "on_emacs_%s" % (name))
        if not func:
            return msg.debug('unknown name', name, 'data:', data)
        func(data)

    def on_emacs_change(self, req):
        path = req['full_path']
        if not path:
            return

        self.emacs_bufs[path][0] = req['after']

        buf = self.get_buf_by_path(path)
        if not buf:
            msg.debug("buf not found for path %s. not sending patch" % path)
            return

        view = self.get_view(buf['id'])
        if not view:
            msg.debug("view not found for %s %s" % (buf['id'], buf['path']))

        self.BUFS_CHANGED.append(buf['id'])

    def on_emacs_buffer_list_change(self, req):
        added = req.get('added') or {}
        for path, text in added.iteritems():
            buf = self.get_buf_by_path(path)
            self.emacs_bufs[path][0] = text
            if not buf:
                msg.debug('no buf for path %s' % path)
                self.create_buf(path, text)
                continue
            if self.views.get(buf['id']) is None:
                self.get_view(buf['id'])
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

    def on_room_info(self, room_info):
        super(Protocol, self).on_room_info(room_info)
        emacs.put('room_info', {'project_path': G.PROJECT_PATH})

    def on_create_buf(self, data):
        super(Protocol, self).on_create_buf(data)
        emacs.put('create_buf', {
            'full_path': utils.get_full_path(data['path']),
            'path': data['path'],
            'username': data.get('username', ''),
        })

    def on_delete_buf(self, data):
        buf = self.FLOO_BUFS[data['id']]
        path = buf['path']
        super(Protocol, self).on_delete_buf(data)
        emacs.put('delete_buf', {
            'full_path': utils.get_full_path(path),
            'path': path,
            'username': data.get('username', ''),
        })
