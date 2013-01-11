from twisted.internet.protocol import Factory

import agent
import editorProtocol


class EditorFactory(Factory):
    def __init__(self):
        Factory.__init__(self)
        self.agent = agent.Agent(self)

    def buildProtocol(self, addr):
        self.protocol = editorProtocol.EditorProtocol(self.agent, "editor")
        return self.protocol
