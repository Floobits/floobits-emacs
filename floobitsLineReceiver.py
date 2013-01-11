import json

from twisted.protocols.basic import LineReceiver


class FloobitsLineReceiver(LineReceiver):
    delimiter = '\n'

    def __init__(self, agent, dispatchPrefix):
        self.dispatchPrefix = dispatchPrefix
        self.agent = agent

    def dispatch(self, event):
        attr = "{prefix}_{event}".format(event=event, prefix=self.dispatchPrefix)
        return getattr(self, attr, None)

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
