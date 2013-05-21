import protocol
import shared as G


emacs = None


class Protocol(protocol.BaseProtocol):
    CLIENT = 'Emacs'

    def __init__(self, *args, **kwargs):
        global emacs
        super(Protocol, self).__init__(*args, **kwargs)
        emacs = G.EMACS

    def on_room_info(self, room_info):
        super(Protocol, self).on_room_info(room_info)
        emacs.put('room_info', {'project_path': G.PROJECT_PATH})

    def get_view(self, buf_id):
        return None
