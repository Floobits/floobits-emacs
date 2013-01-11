import floobitsLineReceiver


class CloudProtocol(floobitsLineReceiver.FloobitsLineReceiver):
    """Talks to the cloud"""

    # all floobits tracked buffers
    bufs = {}
    VERSION = "0.01"

    def __init__(self, factory, send_to_editor):
        floobitsLineReceiver.FloobitsLineReceiver.__init__(self, factory)
        self.authed = False
        self.perms = []
        self.send_to_editor = send_to_editor

    def connectionMade(self):
        self.factory.onConnection()
        print('connected to server')

    def connectionLost(self, reason):
        print('connection to server lost', reason)

    def floo_room_info(self, req, line):
        self.perms = req['perms']
        self.bufs = req['bufs']
        self.tree = req['tree']
        self.send_to_editor(req['bufs'])

    def floo_patch(self):
        pass

    def floo_get_buf(self):
        pass

    def floo_create_buf(self):
        pass

    def floo_rename_buf(self):
        pass

    def floo_delete_buf(self):
        pass

    def floo_join(self):
        pass

    def floo_part(self):
        pass

    def floo_highlight(self):
        pass

    def floo_error(self):
        pass

    def floo_disconnect(self, req, raw):
        print('diconnected: ', req['reason'])

    def floo_msg(self):
        pass
