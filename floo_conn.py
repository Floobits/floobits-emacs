from twisted.internet.protocol import ReconnectingClientFactory

import settings
from floobits_line_receiver import FloobitsLineReceiver


class FloobitsConnProtocol(FloobitsLineReceiver):
    """Talks to the cloud"""

    # all floobits tracked buffers
    bufs = {}
    VERSION = "0.01"

    def __init__(self, factory, send_to_editor):
        self.authed = False
        self.perms = []
        FloobitsLineReceiver.__init__(self, factory)
        self.send_to_editor = send_to_editor

    def connectionMade(self):
        self.factory.onConnection()
        print('connected to server')

    def connectionLost(self, reason):
        print('connection to server lost', reason)

    def floo_room_info(self, req, line):
        self.perms = req['perms']
        self.send_to_editor(req['bufs'])

    def floo_patch(self):
        pass

    def floo_get_buf(self):
        pass

    def floo_create_buf(self):
        pass

    def floo_rename_buf(self):
        pass

    def floo_delete_buf(self):
        pass

    def floo_join(self):
        pass

    def floo_part(self):
        pass

    def floo_highlight(self):
        pass

    def floo_error(self):
        pass

    def floo_disconnect(self, req, raw):
        print('diconnected: ', req['reason'])

    def floo_msg(self):
        pass


class FloobitsConnFactory(ReconnectingClientFactory):

    def __init__(self, send_to_editor, username, room, owner, secret):
        self._bufIn = []
        self._bufOut = []
        self.send_to_editor = send_to_editor
        self.username = username
        self.secret = secret
        self.room = room
        self.room_owner = owner or settings.username

    def startedConnecting(self, connector):
        print 'Started to connect.'

    def buildProtocol(self, addr):
        self.resetDelay()
        self.protocol = FloobitsConnProtocol(self, self.send_to_editor)
        return self.protocol

    def onConnection(self):
        auth = {
            'username': self.username,
            'secret': self.secret,
            'version': self.protocol.VERSION,
            'room': self.room,
            'room_owner': self.room_owner
        }
        print('joining room %s/%s' % (self.room_owner, self.room))
        self.protocol.sendLine(auth)

        # while len(self._bufIn):
        #     line = self._bufIn.pop()
        #     print('sending line', line)
        #     self.protocol.sendLine(line)

    def clientConnectionLost(self, connector, reason):
        print 'Lost connection.  Reason:', reason
        self.authed = False
        ReconnectingClientFactory.clientConnectionLost(self, connector, reason)

    def clientConnectionFailed(self, connector, reason):
        print 'Connection failed. Reason:', reason
        ReconnectingClientFactory.clientConnectionFailed(self, connector, reason)
