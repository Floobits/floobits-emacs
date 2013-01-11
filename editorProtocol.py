from  twisted.internet import reactor

from floo_conn import FloobitsConnFactory
from floobits_line_receiver import FloobitsLineReceiver


class FlooProtocol(FloobitsLineReceiver):
    """Talks to editors"""

    def __init__(self, factory):
        FloobitsLineReceiver.__init__(self, factory)
        self.floo = None

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
        print(raw)
        self.floo = FloobitsConnFactory(self.factory.sendToEditor, username, room, owner, secret)
        reactor.connectTCP("staging.floobits.com", 3148, self.floo)
