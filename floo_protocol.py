import json

from twisted import reactor
import dmp

from utils import FloobitsLineReceiver
from floo_conn import FloobitsConnFactory

BUFS = {}


class FlooProtocol(FloobitsLineReceiver):

    def __init__(self, factory):
        FloobitsLineReceiver.__init__(self, factory)
        self.floo = None

    def connectionMade(self):
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
