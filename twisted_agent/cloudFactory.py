from twisted.internet.protocol import ReconnectingClientFactory

import cloudProtocol


class CloudFactory(ReconnectingClientFactory):
    """manages cloud connections (ie, to the backend service)"""

    def __init__(self, agent):
        self.agent = agent
        self.reconnect = True

    def startedConnecting(self, connector):
        print 'Started to connect.'

    def buildProtocol(self, addr):
        self.resetDelay()
        self.protocol = cloudProtocol.CloudProtocol(self.agent, "cloud")
        return self.protocol

    def clientConnectionLost(self, connector, reason):
        print 'Lost connection.  Reason:', reason
        if self.reconnect:
            ReconnectingClientFactory.clientConnectionLost(self, connector, reason)

    def clientConnectionFailed(self, connector, reason):
        print 'Connection failed. Reason:', reason
        ReconnectingClientFactory.clientConnectionFailed(self, connector, reason)
