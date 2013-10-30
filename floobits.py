from floo.common import migrations
from floo.common import reactor
from floo.common import msg
from floo.common import utils
from floo.common import shared as G
from floo.common.handlers import base
from floo.common.protocols import emacs_proto

G.__VERSION__ = '0.03'
G.__PLUGIN_VERSION__ = '0.2'
utils.reload_settings()

# enable debug with let floo_log_level = 'debug'
floo_log_level = 'debug'
msg.LOG_LEVEL = msg.LOG_LEVELS.get(floo_log_level.upper(), msg.LOG_LEVELS['MSG'])

agent = None

migrations.rename_floobits_dir()
migrations.migrate_symlinks()

if __name__ == '__main__':
    emacs = base.BaseHandler()
    emacs.PROTOCOL = emacs_proto.EmacsProtocol
    r = reactor.install(20)
    r.listen(emacs, 'localhost', 4567)
    G.EMACS = emacs
    r.block()
