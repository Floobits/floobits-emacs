from common import msg
from common import utils


class View(object):
    """editors representation of the buffer"""

    def __init__(self, emacs, buf, emacs_buf=None):
        self.buf = buf
        self._emacs_buf = emacs_buf
        self._emacs = emacs
        if emacs_buf is None:
            emacs.send({
                'name': 'create_view',
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

    def update(self, data):
        self.buf = data
        self.set_text(self.buf['buf'])

    def set_text(self, text):
        self.emacs_buf = text
        self._emacs.send({
            'name': 'get_buf',
            'id': self.buf['id'],
            'full_path': utils.get_full_path(self.buf['path']),
            'buf': text,
        })

    def set_read_only(self, state):
        pass

    def apply_patches(self, buf, patches, username):
        self.emacs_buf = buf['buf']
        self._emacs.send({
            'name': 'edit',
            'id': self.buf['id'],
            'full_path': utils.get_full_path(self.buf['path']),
            'edits': patches,
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

    def set_status(self, status):
        msg = {
            'name': "message",
            'message': status
        }
        self._emacs.send(msg)
