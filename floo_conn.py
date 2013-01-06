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
        FloobitsLineReceiver.__init__(self, factory)
        self.send_to_editor = send_to_editor

    def connectionMade(self):
        self.factory.onConnection()
        print('connected to server')

    def connectionLost(self, reason):
        print('connection to server lost', reason)

    def auth(self, room, owner):
        self.sendLine({
            'username': settings.username,
            'secret': settings.secret,
            'version': self.VERSION,
            'room': room,
            'room_owner': owner
        })

    def floo_room_info(self, req, line):
        print req
        self.sendToEditor(req['bufs'])

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

    def __init__(self, send_to_editor, room, owner):
        self._bufIn = []
        self._bufOut = []
        self.send_to_editor = send_to_editor
        self.secret = None
        self.username = None
        self.authed = False
        self.room = room
        self.room_owner = owner

    def startedConnecting(self, connector):
        print 'Started to connect.'

    def buildProtocol(self, addr):
        self.resetDelay()
        self.protocol = FloobitsConnProtocol(self, self.send_to_editor)
        return self.protocol

    def onConnection(self):
        self.protocol.auth(self.room, self.room_owner)

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
