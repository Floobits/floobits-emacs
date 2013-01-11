import json

from twisted.protocols.basic import LineReceiver


class FloobitsLineReceiver(LineReceiver):
    delimiter = '\n'

    def __init__(self, agent, dispatchPrefix):
        self.dispatchPrefix = dispatchPrefix
        self.agent = agent
        self.version = getattr(self.agent, "{p}_VERSION".format(p=self.dispatchPrefix.upper()))

    def dispatch(self, event, version):
        if self.version != version:
            return None

        attr = "{p}_{e}".format(p=self.dispatchPrefix, e=event)
        return getattr(self.agent, attr, None)

    def lineReceived(self, line):
        req = json.loads(line)

        event = req.get('name')
        if not event:
            raise AttributeError("no name key in req", req)

        version = req.get('version')
        if not version:
            raise AttributeError("no name version in req", req)

        method = self.dispatch(event, version)
        if not method:
            raise ValueError("Could not dispatch {e} for version {v}".format(e=event, v=version))

        return method(req)

    def sendLine(self, line):
        if not isinstance(line, basestring):
            line = json.dumps(line)
        LineReceiver.sendLine(self, line)
