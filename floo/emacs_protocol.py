import protocol
import shared as G


emacs = G.EMACS


class Protocol(protocol.BaseProtocol):
    CLIENT = 'Emacs'

    def on_room_info(self, room_info):
        super(Protocol, self).on_room_info(room_info)
        import pdb; pdb.set_trace()
        emacs.put('room_info', {'project_path': G.PROJECT_PATH})

    def get_view(buf_id):
        return None
