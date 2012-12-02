from twisted.protocols.basic import LineReceiver
from twisted.internet.protocol import Factory, ReconnectingClientFactory
from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.internet import reactor

import dmp


class FloobitsClientProtocol(LineReceiver):
    def __init__(self, factory):
        self.factory = factory

    def connectionMade(self):
        self.factory.numProtocols = self.factory.numProtocols + 1
        self.floo = reactor.connectTCP("staging.floobits.com", 3148, FloobitsClientFactory())

    def lineReceived(self, line):
        print line

    def connectionLost(self, reason):
        self.factory.numProtocols = self.factory.numProtocols - 1

    def dataReceived(self, data):
        print('received data', data)


class FloobitsClientFactory(ReconnectingClientFactory):
    def startedConnecting(self, connector):
        print 'Started to connect.'

    def buildProtocol(self, addr):
        print 'Connected.'
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
        self.factory.numProtocols = self.factory.numProtocols + 1
        self.floo = FloobitsClientFactory()
        reactor.connectTCP("staging.floobits.com", 3148, self.floo)

    def connectionLost(self, reason):
        self.factory.numProtocols = self.factory.numProtocols - 1

    def lineReceived(self, line):
        pass

    def dataReceived(self, data):
        self.transport.write(data)


class AgentFactory(Factory):
    def buildProtocol(self, addr):
        return AgentProtocol(self)

    def authorize(self, data):
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

TCP4ServerEndpoint(reactor, 8007).listen(AgentFactory)
reactor.run()
