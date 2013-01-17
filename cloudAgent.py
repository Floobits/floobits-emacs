class CloudAgent():
    CLOUD_VERSION = "0.01"

    def cloud_room_info(self, req):
        self.perms = req['perms']
        self.bufs = req['bufs']
        self.tree = req['tree']
        self.sendToEditor(req)

    def cloud_patch(self, req):
        pass

    def cloud_get_buf(self, req):
        pass

    def cloud_create_buf(self, req):
        pass

    def cloud_rename_buf(self, req):
        pass

    def cloud_delete_buf(self, req):
        pass

    def cloud_join(self, req):
        pass

    def cloud_part(self, req):
        pass

    def cloud_highlight(self, req):
        pass

    def cloud_error(self, req):
        pass

    def cloud_disconnect(self, req, raw):
        print('diconnected: ', req['reason'])

    def cloud_msg(self):
        pass
