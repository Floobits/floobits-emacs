from floo import emacs_handler
from floo.common import migrations
from floo.common import reactor
from floo.common import msg
from floo.common import utils
from floo.common import shared as G

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
    r = reactor.install(20)
    emacs = emacs_handler.EmacsHandler()
    r.listen(emacs, 'localhost', 4567)

    def cb():
        print("Now_listening")
    utils.set_timeout(cb, 100)
    r.block()
