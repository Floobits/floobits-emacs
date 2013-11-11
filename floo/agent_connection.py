from floo.common.handlers import floo_handler
from floo.common import msg, utils, shared as G


class AgentConnection(floo_handler.FlooHandler):

    def __init__(self, owner, workspace, emacs_handler, get_bufs=True):
        super(AgentConnection, self).__init__(owner, workspace, get_bufs)
        self.emacs_handler = emacs_handler

    @property
    def client(self):
        return 'Emacs'

    def get_view(self, buf_id):
        return self.emacs_handler.get_view(buf_id)

    def ok_cancel_dialog(self, prompt, cb):
        return self.emacs_handler.get_input(prompt, "", cb=lambda data: cb(data['response']), y_or_n=True)

    def to_emacs(self, name, data):
        data['name'] = name
        self.emacs_handler.send(data)

    def _on_room_info(self, data):
        super(AgentConnection, self)._on_room_info(data)
        self.to_emacs('room_info', {
            'perms': data['perms'],
            'project_path': G.PROJECT_PATH,
            'workspace_name': data['room_name'],
        })

    def _on_create_buf(self, data):
        super(AgentConnection, self)._on_create_buf(data)
        self.to_emacs('create_buf', {
            'full_path': utils.get_full_path(data['path']),
            'path': data['path'],
            'username': data.get('username', ''),
        })

    def _on_delete_buf(self, data):
        buf_id = int(data['id'])
        buf = self.bufs[buf_id]
        path = buf['path']
        try:
            super(AgentConnection, self)._on_delete_buf(data)
        except Exception as e:
            msg.debug('Unable to delete buf %s: %s' % (path, str(e)))
        else:
            self.to_emacs('delete_buf', {
                'full_path': utils.get_full_path(path),
                'path': path,
                'username': data.get('username', ''),
            })

    def _on_rename_buf(self, data):
        buf = self.bufs[int(data['id'])]
        # This can screw up if someone else renames the buffer around the same time as us. Oh well.
        buf = self.get_buf_by_path(utils.get_full_path(data['path']))
        if buf:
            return super(AgentConnection, self)._on_rename_buf(data)
        msg.debug('We already renamed %s. Skipping' % buf['path'])

    def _on_highlight(self, data):
        buf = self.bufs[data['id']]
        # TODO: save highlights for when user opens the buffer in emacs
        self.to_emacs('highlight', {
            'full_path': utils.get_full_path(buf['path']),
            'ranges': data['ranges'],
            'user_id': data['user_id'],
            'username': data.get('username', 'unknown user'),
            'ping': data.get('ping', False)
        })

    def _on_msg(self, data):
        msg.log('msg')
