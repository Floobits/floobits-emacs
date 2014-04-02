import base64
import webbrowser

from floo.common.handlers import floo_handler
from floo.common import msg, utils, shared as G


class AgentConnection(floo_handler.FlooHandler):

    def __init__(self, owner, workspace, emacs_handler, get_bufs=True):
        super(AgentConnection, self).__init__(owner, workspace, get_bufs)
        self.emacs_handler = emacs_handler

    def stop(self):
        super(AgentConnection, self).stop()
        self.emacs_handler.stop()

    def get_view(self, buf_id):
        return self.emacs_handler.get_view(buf_id)

    def ok_cancel_dialog(self, prompt, cb):
        return self.emacs_handler.get_input(prompt, "", cb=lambda data: cb(data['response']), y_or_n=True)

    def to_emacs(self, name, data):
        data['name'] = name
        self.emacs_handler.send(data)

    def stomp_prompt(self, changed_bufs, missing_bufs, cb):
        prompt = 'The workspace is out of sync.\n\n'
        choices = [['overwrite-remote', 0], ['overwrite-local', 1], ['cancel', 2]]

        def handle_choice(data, *args, **kwargs):
            for c in choices:
                if c[0] == data.get('response'):
                    return cb(c[1])
            return cb(-1)

        def pluralize(arg):
            return len(arg) > 1 and 's' or ''

        diffs = changed_bufs + missing_bufs
        overwrite_local = ''
        overwrite_remote = ''

        if changed_bufs:
            if len(diffs) < 5:
                changed = ', '.join([buf['path'] for buf in changed_bufs])
            else:
                changed = len(changed_bufs)
            overwrite_local += 'Overwrite %s' % changed
            overwrite_remote += 'Upload %s' % changed

            if missing_bufs:
                if len(diffs) < 5:
                    missing = ', '.join([buf['path'] for buf in missing_bufs])
                    overwrite_local += ' and create %s' % missing
                    overwrite_remote += ' and remove %s' % missing
                else:
                    overwrite_local += ' and create %s local file%s.' % (len(missing_bufs), pluralize(missing_bufs))
                    overwrite_remote += ' and remove %s remote file%s.' % (len(missing_bufs), pluralize(missing_bufs))
            elif len(diffs) >= 5:
                overwrite_local += ' local file%s.' % pluralize(changed_bufs)
                overwrite_remote += ' file%s.' % pluralize(changed_bufs)
        elif missing_bufs:
            if len(diffs) < 5:
                missing = ', '.join([buf['path'] for buf in missing_bufs])
                overwrite_local += 'Create %s.' % missing
                overwrite_remote += 'Remove %s.' % missing
            else:
                overwrite_local += 'Create %s local file%s.' % (len(missing_bufs), pluralize(missing_bufs))
                overwrite_remote += 'Remove %s remote file%s.' % (len(missing_bufs), pluralize(missing_bufs))

        prompt += 'overwrite-remote: %s\n' % overwrite_remote
        prompt += 'overwrite-local: %s\n' % overwrite_local
        prompt += 'cancel: Disconnect and resolve conflict manually.\n\n'
        prompt += 'Action: '

        self.emacs_handler.get_input(prompt, 'overwrite-', cb=handle_choice, choices=choices, timeout=60*1000)

    @utils.inlined_callbacks
    def prompt_join_hangout(self, hangout_url):
        join = yield self.ok_cancel_dialog, 'This workspace is being edited in a hangout. Would you like to join the hangout?'
        if not join:
            return
        try:
            webbrowser.open(hangout_url, new=2, autoraise=True)
        except Exception as e:
            msg.error("Couldn't open a browser: %s" % (str(e)))

    def _on_room_info(self, data):
        def send_room_info():
            self.to_emacs('room_info', {
                'perms': data['perms'],
                'project_path': G.PROJECT_PATH,
                'workspace_name': data['room_name']
            })
        self.once('room_info', send_room_info)
        super(AgentConnection, self)._on_room_info(data)

    def _on_create_buf(self, data):
        if data['encoding'] == 'base64':
            data['buf'] = base64.b64decode(data['buf'])
        self.bufs[data['id']] = data
        self.paths_to_ids[data['path']] = data['id']
        abs_path = utils.get_full_path(data['path'])

        self.to_emacs('create_buf', {
            'full_path': utils.get_full_path(data['path']),
            'path': data['path'],
            'username': data.get('username', ''),
        })

        if abs_path not in self.emacs_handler.emacs_bufs:
            utils.save_buf(data)
            return
        text = self.emacs_handler.emacs_bufs.get(abs_path)[0]
        if text == data['buf']:
            return
        self.emacs_handler.bufs_changed.append(data['id'])

    def _on_delete_buf(self, data):
        buf_id = int(data['id'])
        buf = self.bufs[buf_id]
        path = buf['path']
        try:
            super(AgentConnection, self)._on_delete_buf(data)
        except Exception as e:
            msg.debug('Unable to delete buf %s: %s' % (path, str(e)))
        else:
            self.to_emacs('delete_buf', {
                'full_path': utils.get_full_path(path),
                'path': path,
                'username': data.get('username', ''),
            })

    def _on_rename_buf(self, data):
        # This can screw up if someone else renames the buffer around the same time as us. Oh well.
        msg.debug('asdf %s' % data)
        buf = self.get_buf_by_path(utils.get_full_path(data['old_path']))
        if buf:
            return super(AgentConnection, self)._on_rename_buf(data)
        msg.debug('We already renamed %s. Skipping' % data['old_path'])

    def _on_highlight(self, data):
        buf = self.bufs[data['id']]
        # TODO: save highlights for when user opens the buffer in emacs
        self.to_emacs('highlight', {
            'full_path': utils.get_full_path(buf['path']),
            'ranges': data['ranges'],
            'user_id': data['user_id'],
            'username': data.get('username', 'unknown user'),
            'following': data.get('following', False),
            'ping': data.get('ping', False)
        })

    def _on_msg(self, data):
        msg.log('msg')
