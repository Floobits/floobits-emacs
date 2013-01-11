import json

from twisted.protocols.basic import LineReceiver


class FloobitsLineReceiver(LineReceiver):
    delimiter = '\n'

    def __init__(self, agent, dispatchPrefix):
        self.dispatchPrefix = dispatchPrefix
        self.agent = agent

    def dispatch(self, event):
        attr = "{prefix}_{event}".format(event=event, prefix=self.dispatchPrefix)
        return getattr(self.agent, attr, None)

    def lineReceived(self, line):
        req = json.loads(line)

        event = req.get('name')
        if not event:
            raise AttributeError("no name key in req", req)

        method = self.dispatch(event)
        if method:
            return method(req, line)

        raise ValueError("Could not dispatch event", event)

    def sendLine(self, line):
        if not isinstance(line, basestring):
            line = json.dumps(line)
        LineReceiver.sendLine(self, line)
