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

    def __init__(self, buf, emacs_buf=""):
        self.buf = buf
        self.emacs_buf = emacs_buf

    def __repr__(self):
        return '%s %s %s' % (self.native_id, self.buf['id'], self.buf['path'])

    def __str__(self):
        return repr(self)

    @property
    def native_id(self):
        return self.buf['id']

    def is_loading(self):
        return False

    def get_text(self):
        return self.emacs_buf

    def set_text(self, text):
        self.emacs_buf = text
        emacs.put('get_buf', {
            'id': self.buf['id'],
            'full_path': utils.get_full_path(self.buf['path']),
            'buf': self.emacs_buf,
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
        emacs = G.EMACS
        self.views = {}

    def get_view(self, buf_id):
        return self.views.get(buf_id)

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

        buf = self.get_buf_by_path(path)
        if not buf:
            print("buf not found for path %s. not sending patch" % path)
            return

        view = self.get_view(buf['id'])
        if not view:
            msg.log("oops this is bad. view not found for %s %s" % (buf['id'], buf['path']))
        view.emacs_buf = req['after']

        self.BUFS_CHANGED.append(buf['id'])

    def on_emacs_buffer_list_change(self, req):
        msg.debug("buffers changed", req)
        added = req.get('added') or {}
        for path, text in added.iteritems():
            buf = self.get_buf_by_path(path)
            if not buf:
                msg.debug('no buf for path %s' % path)
                # TODO: check is_shared and create_buf if we need to
                continue
            if self.get_view(buf['id']) is None:
                self.views[buf['id']] = View(buf, text)
            else:
                msg.debug('view for buf %s already exists. this is not good. we got out of sync')
                self.views[buf['id']].emacs_buf = text

        deleted = req.get('deleted') or []
        for path in deleted:
            buf = self.get_buf_by_path(path)
            if buf:
                del self.views[buf['id']]

        seen = set()
        current = req.get('current') or []
        for path in current:
            buf = self.get_buf_by_path(path)
            if not buf:
                continue
            if buf['id'] in self.views:
                seen.add(buf['id'])
            else:
                msg.debug('We should have buffer %s in our views but we don\'t' % buf['path'])

        for buf_id, view in self.views.iteritems():
            if buf_id not in seen:
                msg.debug('We should not have buffer %s in our views but we do.' % view.buf['path'])

    def on_room_info(self, room_info):
        super(Protocol, self).on_room_info(room_info)
        emacs.put('room_info', {'project_path': G.PROJECT_PATH})
