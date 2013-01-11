
import floobitsLineReceiver


class EditorProtocol(floobitsLineReceiver.FloobitsLineReceiver):
    """Talks to editors"""

    def connectionMade(self):
        """ set up a connection to the backend """
        print("received connection from editor")

    def connectionLost(self, reason):
        print 'connection lost', reason
        self.floo.stopTrying()
        self.floo.doStop()

    def floo_auth(self, req, raw):
        room = req['room']
        username = req['username']
        owner = req.get('room_owner', username)
        secret = req['secret']

        self.agent.auth(username, room, owner, secret)
