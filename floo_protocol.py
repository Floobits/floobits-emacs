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
        owner = req.get('room_owner')
        self.floo = FloobitsConnFactory(self.factory.sendToEditor, room, owner)
        reactor.connectTCP("staging.floobits.com", 3148, self.floo)
