import sys

import cloudAgent
from editors import emacs
import utils


class Agent(cloudAgent.CloudAgent, emacs.Emacs):

    def __init__(self, editorFactory):
        self.editorFactory = editorFactory
        self.bufs = {}
        self.cloudFactory = None
        self.username = None
        self.secret = None
        self.room = None
        self.room_owner = None

    def sendToCloud(self, req):
        #TODO: the factory or protocol may not exist
        req['version'] = self.CLOUD_VERSION
        self.cloudFactory.protocol.sendLine(req)

    def sendToEditor(self, req):
        req['version'] = self.EDITOR_VERSION
        self.editorFactory.protocol.sendLine(req)

    def onCloudConnection(self):
        print('joining room %s/%s' % (self.room_owner, self.room))
        auth = {
            'username': self.username,
            'secret': self.secret,
            'room': self.room,
            'room_owner': self.room_owner,
            'platform': sys.platform,
            'client': self.CLIENT
        }
        self.sendToCloud(auth)

    def isShared(self, p):
        p = utils.unfuck_path(p)
        return self.basePath == p[:len(self.basePath)]

    def getBufByPath(self, filepath):
        if not self.isShared(filepath):
            return

        filepath = filepath[len(self.basePath) + 1:]
        for bufId, buf in self.bufs.iteritems():
            if buf['path'] == filepath:
                return buf
