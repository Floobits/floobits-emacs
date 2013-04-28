import floobitsLineReceiver


class CloudProtocol(floobitsLineReceiver.FloobitsLineReceiver):
    """Talks to the cloud"""

    def dispatch(self, event):
        """send stuff back up the agent which keeps track of buffers and stuff"""
        return getattr(self.agent, "cloud_{event}".format(event=event), None)

    def connectionMade(self):
        print('connected to server')
        self.agent.onCloudConnection()

    def connectionLost(self, reason):
        print('connection to server lost', reason)
