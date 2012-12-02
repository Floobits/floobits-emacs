from twisted.protocols.basic import LineReceiver
from twisted.internet.protocol import Factory
from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.internet import reactor


class AgentProtocol(LineReceiver):

    def __init__(self, factory):
        self.factory = factory

    def connectionMade(self):
        self.factory.numProtocols = self.factory.numProtocols + 1

    def connectionLost(self, reason):
        self.factory.numProtocols = self.factory.numProtocols - 1

    def lineReceived(self, line):
        pass

    def dataReceived(self, data):
        self.transport.write(data)


class AgentFactory(Factory):
    def buildProtocol(self, addr):
        return AgentProtocol(self)

endpoint = TCP4ServerEndpoint(reactor, 8007)
endpoint.listen(AgentFactory)
reactor.run()
