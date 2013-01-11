from twisted.internet.protocol import ReconnectingClientFactory

import settings
import cloudProtocol


class CloudFactory(ReconnectingClientFactory):
    """manages cloud connections (ie, to the backend service)"""

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
        self.protocol = cloudProtocol.CloudProtocol(self, self.send_to_editor)
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
