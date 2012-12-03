import json

from twisted.protocols.basic import LineReceiver
from twisted.internet.protocol import Factory, ReconnectingClientFactory
from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.internet import reactor

import dmp

bufs = {}


class FloobitsLineReceiver(LineReceiver):
    delimiter = '\n'

    def __init__(self, factory):
        self.factory = factory

    def lineReceived(self, line):
        req = json.loads(line)

        event_name = req.get('name')
        if not event_name:
            raise ValueError("no name key in req", req)

        method = getattr(self, "floo_%s" % req['name'], None)
        if method:
            return method(req, line)

        raise ValueError("no name key in req", req)


class FloobitsFlooProtocol(FloobitsLineReceiver):

    def floo_room_info(self, req, line):
        global bufs
        bufs = req['bufs']
        self.factory.sendToEditor(req['bufs'])

    def connectionMade(self):
        self.factory.onConnection()
        print('connected to server')

    def connectionLost(self, reason):
        print('connection to server lost', reason)


class FloobitsFlooFactory(ReconnectingClientFactory):

    def __init__(self, sendToEditor):
        self.buf_in = []
        self.buf_out = []
        self.sendToEditor = sendToEditor

    def startedConnecting(self, connector):
        print 'Started to connect.'

    def buildProtocol(self, addr):
        self.resetDelay()
        self.protocol = FloobitsFlooProtocol(self)
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


class EditorProtocol(FloobitsLineReceiver):

    def __init__(self, factory):
        FloobitsLineReceiver.__init__(self, factory)
        self.floo = None

    def connectionMade(self):
        self.floo = FloobitsFlooFactory(self.factory.sendToEditor)
        reactor.connectTCP("staging.floobits.com", 3148, self.floo)

    def connectionLost(self, reason):
        print 'connection lost', reason
        self.floo.stopTrying()
        self.floo.doStop()

    def floo_auth(self, req, raw):
        if 'room_owner' not in req:
            req['room_owner'] = req['username']
            return self.floo.sendLine(json.dumps(req))

        self.floo.sendLine(raw)

    def get_buf(self):
        pass

    def room_info(self):
        pass

    def join(self):
        pass

    def part(self):
        pass

    def highlight(self):
        pass


class AgentFactory(Factory):
    def sendToEditor(self, line):
        #TODO: make sure we have a protocol
        if not isinstance(line, basestring):
            line = json.dumps(line)
        self.protocol.sendLine(line)
        # self.protocol.transport.write(line)

    def buildProtocol(self, addr):
        self.protocol = EditorProtocol(self)
        return self.protocol

TCP4ServerEndpoint(reactor, 4567).listen(AgentFactory())
reactor.run()
