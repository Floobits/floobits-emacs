from twisted.internet.protocol import Factory

import editorProtocol


class EditorFactory(Factory):
    def __init__(self, agent, *args, **kwargs):
        Factory.__init__(self, *args, **kwargs)
        self.agent = agent

    def sendToEditor(self, line):
        #TODO: make sure we have a protocol
        self.protocol.sendLine(line)

    def buildProtocol(self, addr):
        self.protocol = editorProtocol.EditorProtocol(self)
        return self.protocol
