from twisted.internet.protocol import ReconnectingClientFactory

from utils import FloobitsLineReceiver


class FloobitsConnProtocol(FloobitsLineReceiver):

    def floo_room_info(self, req, line):
        global bufs
        bufs = req['bufs']
        self.factory.sendToEditor(req['bufs'])

    def connectionMade(self):
        self.factory.onConnection()
        print('connected to server')

    def connectionLost(self, reason):
        print('connection to server lost', reason)


class FloobitsConnFactory(ReconnectingClientFactory):

    def __init__(self, sendToEditor):
        self.buf_in = []
        self.buf_out = []
        self.sendToEditor = sendToEditor

    def startedConnecting(self, connector):
        print 'Started to connect.'

    def buildProtocol(self, addr):
        self.resetDelay()
        self.protocol = FloobitsConnProtocol(self)
        return self.protocol

    def onConnection(self):
        while len(self.buf_in):
            line = self.buf_in.pop()
            print('sending line', line)
            self.protocol.sendLine(line)

    def sendLine(self, line):
        if not self.protocol:
            self.buf_in.append(line)
            print('not connected yet!')
            return

        self.protocol.sendLine(line)

    def clientConnectionLost(self, connector, reason):
        print 'Lost connection.  Reason:', reason
        ReconnectingClientFactory.clientConnectionLost(self, connector, reason)

    def clientConnectionFailed(self, connector, reason):
        print 'Connection failed. Reason:', reason
        ReconnectingClientFactory.clientConnectionFailed(self, connector, reason)
