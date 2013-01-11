import json

from twisted.protocols.basic import LineReceiver


class FloobitsLineReceiver(LineReceiver):
    delimiter = '\n'

    def __init__(self, agent):
        self.agent = agent

    def dispatch(self, event):
        return getattr(self, "floo_{event}".format(event=event), None)

    def lineReceived(self, line):
        req = json.loads(line)

        event_name = req.get('name')
        if not event_name:
            raise ValueError("no name key in req", req)

        method = self.dispatch(req['name'])
        if method:
            return method(req, line)

        raise ValueError("no name key in req", req)

    def sendLine(self, line):
        if not isinstance(line, basestring):
            line = json.dumps(line)
        LineReceiver.sendLine(self, line)
