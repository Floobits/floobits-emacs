import os
import json
import socket
import Queue
import sys
import time

import editor
from floo.common.handlers import floo_handler
from floo.common import cert, msg, utils, shared as G


class AgentConnection(floo_handler.FlooHandler):

    def __init__(self, owner, workspace, emacs_handler, get_bufs=True):
        super(AgentConnection, self).__init__(owner, workspace, get_bufs)
        self.emacs_handler = emacs_handler
        if sys.version_info[0] == 2 and sys.version_info[1] == 6:
            # Work around http://bugs.python.org/issue11326
            msg.error('Disabling SSL to work around a bug in Python 2.6. Please upgrade your Python to get SSL. See http://bugs.python.org/issue11326')
            G.SECURE = False
            G.DEFAULT_PORT = 3148

    def to_emacs(self, data):
        self.emacs_handler.put(data)

    def _on_room_info(self, data):
        super(AgentConnection, self)._on_room_info(data)
        # data['project_path'] = G.PROJECT_PATH
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
        buf = self.FLOO_BUFS[buf_id]
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
        buf = self.FLOO_BUFS[int(data['id'])]
        # This can screw up if someone else renames the buffer around the same time as us. Oh well.
        buf = self.get_buf_by_path(utils.get_full_path(data['path']))
        if not buf:
            return super(AgentConnection, self)._on_rename_buf(data)
        msg.debug('We already renamed %s. Skipping' % buf['path'])

    def _on_highlight(self, data):
        super(AgentConnection, self)._on_highlight(data)
        buf = self.FLOO_BUFS[data['id']]
        # TODO: save highlights for when user opens the buffer in emacs
        self.to_emacs('highlight', {
            'id': buf['id'],
            'full_path': utils.get_full_path(buf['path']),
            'ranges': data['ranges'],
            'user_id': data['user_id'],
            'username': data.get('username', 'unknown user'),
        })

    def _on_msg(self, data):
        msg.log('msg')

    def tick(self):
        self.protocol.push()
        self.select()
        editor.call_timeouts()

    def send_get_buf(self, buf_id):
        req = {
            'name': 'get_buf',
            'id': buf_id
        }
        self.put(req)

    # def send_auth(self):
    #     # TODO: we shouldn't throw away all of this
    #     self.sock_q = Queue.Queue()
    #     self.put({
    #         'username': self.username,
    #         'secret': self.secret,
    #         'room': self.workspace,
    #         'room_owner': self.owner,
    #         'client': self.protocol.CLIENT,
    #         'platform': sys.platform,
    #         'supported_encodings': ['utf8', 'base64'],
    #         'version': G.__VERSION__
    #     })

    def send_msg(self, msg):
        self.put({'name': 'msg', 'data': msg})
        self.protocol.chat(self.username, time.time(), msg, True)


