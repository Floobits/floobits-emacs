import json

from twisted.protocols.basic import LineReceiver


class FloobitsLineReceiver(LineReceiver):
    delimiter = '\n'

    def __init__(self, factory):
        self.factory = factory

    def lineReceived(self, line):
        req = json.loads(line)

        event_name = req.get('name')
        if not event_name:
            raise ValueError("no name key in req", req)

        method = getattr(self, "floo_%s" % req['name'], None)
        if method:
            return method(req, line)

        raise ValueError("no name key in req", req)

    def sendLine(self, line):
        if not isinstance(line, basestring):
            line = json.dumps(line)
        LineReceiver.sendLine(self, line)
