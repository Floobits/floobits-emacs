#!/usr/bin/env python
from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.internet import reactor

import editorFactory

if __name__ == "__main__":
    server = editorFactory.EditorFactory()
    TCP4ServerEndpoint(reactor, 4567).listen(server)
    print('Starting up...')
    reactor.run()
