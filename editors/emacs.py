from  twisted.internet import reactor

import cloudFactory


class Emacs():
    EDITOR_VERSION = "0.01"

    def editor_auth(self, req):
        self.room = req['room']
        self.username = req['username']
        self.room_owner = req.get('room_owner', self.username)
        self.secret = req['secret']
        self.cloudFactory = cloudFactory.CloudFactory(self)
        reactor.connectTCP("staging.floobits.com", 3148, self.cloudFactory)

    def editor_change(self, req):
        print req

    def editor_buffer_list(self, req):
        print req
