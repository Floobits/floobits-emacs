from twisted.internet.protocol import ReconnectingClientFactory

import cloudProtocol


class CloudFactory(ReconnectingClientFactory):
    """manages cloud connections (ie, to the backend service)"""

    def __init__(self, agent):
        self.agent = agent

    def startedConnecting(self, connector):
        print 'Started to connect.'

    def buildProtocol(self, addr):
        self.resetDelay()
        self.protocol = cloudProtocol.CloudProtocol(self.agent, "cloud")
        self.connectDefered = None
        return self.protocol

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
