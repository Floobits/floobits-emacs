class CloudAgent():
    CLOUD_VERSION = "0.01"

    def cloud_room_info(self, req):
        self.perms = req['perms']
        self.bufs = req['bufs']
        self.tree = req['tree']
        #TODO: something!

    def cloud_patch(self):
        pass

    def cloud_get_buf(self):
        pass

    def cloud_create_buf(self):
        pass

    def cloud_rename_buf(self):
        pass

    def cloud_delete_buf(self):
        pass

    def cloud_join(self):
        pass

    def cloud_part(self):
        pass

    def cloud_highlight(self):
        pass

    def cloud_error(self):
        pass

    def cloud_disconnect(self, req, raw):
        print('diconnected: ', req['reason'])

    def cloud_msg(self):
        pass
