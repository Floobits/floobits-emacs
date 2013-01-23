from  twisted.internet import reactor

import dmp

import cloudFactory
import dmp_monkey

dmp_monkey.monkey_patch()


class Emacs():
    EDITOR_VERSION = "0.01"

    def editor_auth(self, req):
        print req
        self.room = req['room']
        self.username = req['username']
        self.room_owner = req.get('room_owner', self.username)
        self.secret = req['secret']
        self.cloudFactory = cloudFactory.CloudFactory(self)
        reactor.connectTCP("staging.floobits.com", 3148, self.cloudFactory)

    def editor_change(self, req):
        if not self.is_shared(req['full_path']):
            return
        print req

    def editor_buffer_list(self, req):
        print req
