from floo import emacs_handler
from floo.common import migrations
from floo.common import reactor
from floo.common import msg
from floo.common import utils
from floo.common import shared as G


def cb():
    print("Now_listening")


def main():
    G.__VERSION__ = '0.03'
    G.__PLUGIN_VERSION__ = '0.2'
    utils.reload_settings()

    # enable debug with let floo_log_level = 'debug'
    floo_log_level = 'debug'
    msg.LOG_LEVEL = msg.LOG_LEVELS.get(floo_log_level.upper(), msg.LOG_LEVELS['MSG'])
    migrations.rename_floobits_dir()
    migrations.migrate_symlinks()

    r = reactor.install(20)
    emacs = emacs_handler.EmacsHandler()
    r.listen(emacs, 'localhost', 4567)
    utils.set_timeout(cb, 100)
    r.block()

if __name__ == '__main__':
    main()
