import dmp


class Buffer(object):
    def __init__(self, name, path=None, text=""):
        self.name = name
        self.path = path
        self.text = text

    def to_json(self):
        pass
