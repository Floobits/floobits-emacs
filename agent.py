from  twisted.internet import reactor, defer

import settings
import cloudFactory
from editors import emacs


class Agent(emacs.Emacs):
    VERSION = "0.01"

    def __init__(self, editorFactory):
        self.editorFactory = editorFactory
        self.bufs = {}
        self.cloudFactory = None
        self.username = None
        self.secret = None
        self.room = None
        self.room_owner = None

    def auth(self, username, room, owner, secret):
        self.username = username
        self.secret = secret
        self.room = room
        self.room_owner = owner or settings.username
        d = defer.Deferred()
        d.addCallback(self._onCloud)
        self.cloudFactory = cloudFactory.CloudFactory(self, d)
        reactor.connectTCP("staging.floobits.com", 3148, self.cloudFactory)

    def sendToCloud(self, req):
        #TODO: the factory or protocol may not exist
        self.cloudFactory.protocol.sendLine(req)

    def sendToEditor(self, req):
        self.editorFactory.protocol.sendLine(req)

    def cloud_room_info(self, req, line):
        self.perms = req['perms']
        self.bufs = req['bufs']
        self.tree = req['tree']
        self.agent.send(req['bufs'])

    def cloud_patch(self):
        pass

    def cloud_get_buf(self):
        pass

    def cloud_create_buf(self):
        pass

    def cloud_rename_buf(self):
        pass

    def cloud_delete_buf(self):
        pass

    def cloud_join(self):
        pass

    def cloud_part(self):
        pass

    def cloud_highlight(self):
        pass

    def cloud_error(self):
        pass

    def cloud_disconnect(self, req, raw):
        print('diconnected: ', req['reason'])

    def cloud_msg(self):
        pass

    def editor_auth(self, req, raw):
        room = req['room']
        username = req['username']
        owner = req.get('room_owner', username)
        secret = req['secret']

        self.sendToCloud(username, room, owner, secret)

    def _onCloud(self):
        auth = {
            'username': self.username,
            'secret': self.secret,
            'version': self.protocol.VERSION,
            'room': self.room,
            'room_owner': self.room_owner
        }
        print('joining room %s/%s' % (self.room_owner, self.room))
        self.sendToCloud(auth)
