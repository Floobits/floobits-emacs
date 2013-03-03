#!/usr/bin/env python
import sys

from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.internet import reactor

import editorFactory


def onErr(err):
	print 'AVENGE MEEEEEEEEE!'
	sys.exit(1)


if __name__ == "__main__":
    server = editorFactory.EditorFactory()
    endpoint = TCP4ServerEndpoint(reactor, 4567)
    listener = endpoint.listen(server)
    listener.addErrback(onErr);

    print('Starting up...')
    reactor.run()
