
import floobitsLineReceiver


class EditorProtocol(floobitsLineReceiver.FloobitsLineReceiver):
    """Talks to editors"""

    def connectionMade(self):
        """ set up a connection to the backend """
        print("received connection from editor")

    def connectionLost(self, reason):
        print 'connection lost', reason
        #TODO: kill the backend conn
        # self.agent.factory()
        # self.floo.stopTrying()
        # self.floo.doStop()
