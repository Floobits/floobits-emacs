from twisted.internet.protocol import Factory
from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.internet import reactor

from floo_protocol import FlooProtocol


class AgentFactory(Factory):
    def sendToEditor(self, line):
        #TODO: make sure we have a protocol
        self.protocol.sendLine(line)

    def buildProtocol(self, addr):
        self.protocol = FlooProtocol(self)
        return self.protocol


def main():
    TCP4ServerEndpoint(reactor, 4567).listen(AgentFactory())
    reactor.run()

if __name__ == "__main__":
    main()
