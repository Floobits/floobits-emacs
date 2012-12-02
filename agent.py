import json

from twisted.protocols.basic import LineReceiver
from twisted.internet.protocol import Factory, ReconnectingClientFactory
from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.internet import reactor

import dmp


class FloobitsClientProtocol(LineReceiver):
    def __init__(self, factory):
        self.factory = factory

    def connectionMade(self):
        print('connected to server')

    def lineReceived(self, line):
        print line

    def connectionLost(self, reason):
        print('connection to server lost', reason)

    def dataReceived(self, data):
        print('received data', data)


class FloobitsClientFactory(ReconnectingClientFactory):
    def startedConnecting(self, connector):
        print 'Started to connect.'

    def buildProtocol(self, addr):
        self.resetDelay()
        return FloobitsClientProtocol(self)

    def clientConnectionLost(self, connector, reason):
        print 'Lost connection.  Reason:', reason
        ReconnectingClientFactory.clientConnectionLost(self, connector, reason)

    def clientConnectionFailed(self, connector, reason):
        print 'Connection failed. Reason:', reason
        ReconnectingClientFactory.clientConnectionFailed(self, connector, reason)


class AgentProtocol(LineReceiver):

    def __init__(self, factory):
        self.factory = factory
        self.floo = None

    def connectionMade(self):
        self.floo = FloobitsClientFactory()
        reactor.connectTCP("staging.floobits.com", 3148, self.floo)

    def connectionLost(self, reason):
        print 'connection lost', reason

    def lineReceived(self, line):
        print('line', line)
        try:
            req = json.loads(line)
        except Exception, e:
            print e
            return

        if 'event' in req and req['event']:
            method = getattr(self.factory, req['event'], None)
            if method:
                return method(req)

        print 'no handler for req: ', line

    def dataReceived(self, data):
        #        self.transport.write(data)
        print('got data ', data)


class AgentFactory(Factory):
    def buildProtocol(self, addr):
        return AgentProtocol(self)

    def auth(self, data):
        pass

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

TCP4ServerEndpoint(reactor, 4567).listen(AgentFactory())
reactor.run()
