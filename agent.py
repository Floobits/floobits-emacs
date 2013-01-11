from  twisted.internet import reactor

import settings
import cloudFactory


class Agent(object):
    def __init__(self, editorFactory):
        self.editorFactory = editorFactory
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
        self.cloudFactory = cloudFactory.CloudFactory(self)
        reactor.connectTCP("staging.floobits.com", 3148, self.cloudFactory)

    def sendCloud(self, req):
        self.cloudFactory.protocol.sendLine(req)

    def sendEditor(self, req):
        self.editorFactory.protocol.sendLine(req)

    def onConnection(self):
        auth = {
            'username': self.username,
            'secret': self.secret,
            'version': self.protocol.VERSION,
            'room': self.room,
            'room_owner': self.room_owner
        }
        print('joining room %s/%s' % (self.room_owner, self.room))
        self.sendCloud(auth)
