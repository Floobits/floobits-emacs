import json

from twisted import reactor

from floo_conn import FloobitsConnFactory
from floobits_line_receiver import FloobitsLineReceiver


class FlooProtocol(FloobitsLineReceiver):
    """Talks to editors"""

    def __init__(self, factory):
        FloobitsLineReceiver.__init__(self, factory)
        self.floo = None

    def connectionMade(self):
        """ set up a connection to the backend """
        self.floo = FloobitsConnFactory(self.factory.sendToEditor)
        reactor.connectTCP("staging.floobits.com", 3148, self.floo)

    def connectionLost(self, reason):
        print 'connection lost', reason
        self.floo.stopTrying()
        self.floo.doStop()

    def floo_auth(self, req, raw):
        if 'room_owner' not in req:
            req['room_owner'] = req['username']
            return self.floo.sendLine(json.dumps(req))

        self.floo.sendLine(raw)
