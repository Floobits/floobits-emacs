import json

from twisted.protocols.basic import LineReceiver


class FloobitsLineReceiver(LineReceiver):
    delimiter = '\n'

    def __init__(self, agent, dispatchPrefix):
        self.dispatchPrefix = dispatchPrefix
        self.agent = agent
        self.version = getattr(self.agent, "{p}_VERSION".format(p=self.dispatchPrefix.upper()))

    def lineReceived(self, line):
        req = json.loads(line)

        event = req.get('name')
        if not event:
            raise AttributeError("no name key in req", req)

        version = req.get('version', None)
        if not version:
            #NOTE: fix this (have the backend send a version string)
            # return None
            pass
            # raise AttributeError("no name version in req", req)

        attr = "{p}_{e}".format(p=self.dispatchPrefix, e=event.replace('-', '_'))
        method = getattr(self.agent, attr, None)
        if not method:
            raise ValueError("Could not dispatch {e} for version {v}".format(e=event, v=version))

        return method(req)

    def sendLine(self, line):
        if isinstance(line, dict):
            line = json.dumps(line)
        LineReceiver.sendLine(self, line)
