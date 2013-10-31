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
        self.user_inputs = {}
        self.user_input_count = 0
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

    def _on_auth(self):
        self.authed = True
        G.JOINED_WORKSPACE = True
        self.retries = self.MAX_RETRIES
        msg.log('Successfully joined workspace %s/%s' % (self.owner, self.workspace))
        if self.__on_auth:
            self.__on_auth(self)
            self.__on_auth = None

    def stop(self, log=True):
        if log:
            msg.log('Disconnecting from workspace %s/%s' % (self.owner, self.workspace))
        editor.cancel_timeout(self.reconnect_timeout)
        self.reconnect_timeout = None
        G.JOINED_WORKSPACE = False
        try:
            self.retries = -1
            self.sock.shutdown(2)
            self.sock.close()
        except Exception:
            return False
        if log:
            msg.log('Disconnected.')
        return True

    def is_ready(self):
        return self.authed

    def put(self, item):
        if not item:
            return
        self.sock_q.put(json.dumps(item) + '\n')
        qsize = self.sock_q.qsize()
        if qsize > 0:
            msg.debug('%s items in q' % qsize)

    def qsize(self):
        return self.sock_q.qsize()

    def reconnect(self):
        try:
            self.sock.close()
        except Exception:
            pass
        self.workspace_info = {}
        self.net_buf = ''
        self.sock = None
        self.authed = False
        G.JOINED_WORKSPACE = False
        self.reconnect_delay *= 1.5
        if self.reconnect_delay > 10000:
            self.reconnect_delay = 10000
        if self.retries > 0:
            msg.log('Floobits: Reconnecting in %sms' % self.reconnect_delay)
            self.reconnect_timeout = editor.set_timeout(self.connect, int(self.reconnect_delay))
        elif self.retries == 0:
            msg.error('Floobits Error! Too many reconnect failures. Giving up.')
            sys.exit(0)
        self.retries -= 1

    def connect(self, cb=None):
        self.stop(False)
        self.empty_selects = 0
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        if self.secure:
            if ssl:
                cert_path = os.path.join(G.COLAB_DIR, 'startssl-ca.pem')
                with open(cert_path, 'wb') as cert_fd:
                    cert_fd.write(cert.CA_CERT.encode('utf-8'))
                self.sock = ssl.wrap_socket(self.sock, ca_certs=cert_path, cert_reqs=ssl.CERT_REQUIRED)
            else:
                msg.log('No SSL module found. Connection will not be encrypted.')
                if self.port == G.DEFAULT_PORT:
                    self.port = 3148  # plaintext port
        msg.debug('Connecting to %s:%s' % (self.host, self.port))
        try:
            self.sock.connect((self.host, self.port))
            if self.secure and ssl:
                self.sock.do_handshake()
        except socket.error as e:
            msg.error('Error connecting:', e)
            self.reconnect()
            return
        self.sock.setblocking(0)
        msg.debug('Connected!')
        self.reconnect_delay = self.INITIAL_RECONNECT_DELAY
        self.send_auth()
        if cb:
            cb()

    def _get_from_queue(self):
        while True:
            try:
                yield self.sock_q.get_nowait()
            except Queue.Empty:
                break

    def handle(self, req):
        self.net_buf += req
        while True:
            before, sep, after = self.net_buf.partition('\n')
            if not sep:
                break
            try:
                data = json.loads(before)
            except Exception as e:
                msg.error('Unable to parse json:', e)
                msg.error('Data:', before)
                raise e
            self.protocol.handle(data)
            self.net_buf = after

