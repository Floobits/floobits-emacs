import os


def unfuck_path(p):
    if not p:
        return ""
    return os.path.normcase(os.path.normpath(p))


def mkdir(path):
    try:
        os.makedirs(path)
    except OSError as e:
        if e.errno != 17:
            raise
