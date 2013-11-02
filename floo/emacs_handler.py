# coding: utf-8

try:
    unicode()
except NameError:
    unicode = str

import os
import json
import re
import hashlib
import webbrowser
from collections import defaultdict
from urllib2 import HTTPError

import agent_connection
import editor

from view import View
from common import api
from common import msg
from common import shared as G
from common import utils
from common import reactor
from floo.common.handlers import base
from emacs_protocol import EmacsProtocol


class EmacsHandler(base.BaseHandler):
    CLIENT = 'Emacs'
    PROTOCOL = EmacsProtocol

    def __init__(self, *args, **kwargs):
        super(EmacsHandler, self).__init__(*args, **kwargs)
        self.agent = None  # agent handler (to the backend connection)
        self.views = {}
        self.user_inputs = {}
        self.user_input_count = 0
        self.emacs_bufs = defaultdict(lambda: [""])
        self.bufs_changed = []
        self.selection_changed = []

    def send_to_floobits(self, data):
        self.agent.send(data)

    def tick(self):
        reported = set()
        while self.bufs_changed:
            buf_id = self.bufs_changed.pop()
            view = self.get_view(buf_id)
            buf = view.buf
            if view.is_loading():
                msg.debug('View for buf %s is not ready. Ignoring change event' % buf['id'])
                continue
            if 'patch' not in self.perms:
                continue
            vb_id = view.native_id
            if vb_id in reported:
                continue
            if 'buf' not in buf:
                msg.debug('No data for buf %s %s yet. Skipping sending patch' % (buf['id'], buf['path']))
                continue

            reported.add(vb_id)
            patch = utils.FlooPatch(view.get_text(), view.buf)
            # Update the current copy of the buffer
            buf['buf'] = patch.current
            buf['md5'] = hashlib.md5(patch.current.encode('utf-8')).hexdigest()
            self.send_to_floobits(patch.to_json())

        while self.selection_changed:
            view, ping = self.selection_changed.pop()
            # consume highlight events to avoid leak
            if 'highlight' not in self.perms:
                continue
            vb_id = view.native_id
            if vb_id in reported:
                continue

            reported.add(vb_id)
            highlight_json = {
                'id': vb_id,
                'name': 'highlight',
                'ranges': view.get_selections(),
                'ping': ping,
            }
            self.send_to_floobits(highlight_json)

    def get_input(self, prompt, initial, cb, *args, **kwargs):
        event = {
            'name': 'user_input',
            'id': self.user_input_count,
            'prompt': prompt,
            'initial': initial,
        }
        if 'choices' in kwargs:
            event['choices'] = kwargs['choices']
        elif 'y_or_n' in kwargs:
            event['y_or_n'] = True
            del kwargs['y_or_n']
        self.send(event)
        self.user_inputs[self.user_input_count] = lambda x: cb(x, *args, **kwargs)
        self.user_input_count += 1

    def on_connect(self):
        msg.log("have an emacs!")

    def remote_connect(self, owner, workspace, get_bufs=True):
        G.PROJECT_PATH = os.path.realpath(G.PROJECT_PATH)
        G.PROJECT_PATH += os.sep
        self.agent = agent_connection.AgentConnection(owner, workspace, self, get_bufs)
        reactor.reactor.connect(self.agent, G.DEFAULT_HOST, G.DEFAULT_PORT, True)
        return self.agent

    def create_view(self, buf, emacs_buf=None):
        v = View(buf, emacs_buf)
        self.views[buf['id']] = v
        return v

    def get_view(self, buf_id):
        """Warning: side effects!"""
        # return self.agent.get_view(buf_id)
        view = self.views.get(buf_id)
        if view:
            return view
        buf = self.agent.bufs[buf_id]
        full_path = utils.get_full_path(buf['path'])
        emacs_buf = self.emacs_bufs.get(full_path)
        if emacs_buf:
            view = self.create_view(buf, emacs_buf)
        return view

    def get_view_by_path(self, path):
        """Warning: side effects!"""

        if not path:
            return None
        buf = self.get_buf_by_path(path)
        if not buf:
            msg.debug("buf not found for path %s" % path)
            return None
        view = self.get_view(buf['id'])
        if not view:
            msg.debug("view not found for %s %s" % (buf['id'], buf['path']))
            return None
        return view

    def update_view(self, data, view):
        view.set_text(data['buf'])

    def _on_user_input(self, data):
        cb_id = int(data['id'])
        cb = self.user_inputs.get(cb_id)
        if cb is None:
            msg.error('cb for input %s is none' % cb_id)
            return
        cb(data)
        del self.user_inputs[cb_id]

    def _on_set_follow_mode(self, req):
        self.follow(bool(req['follow_mode']))

    def _on_change(self, req):
        path = req['full_path']
        view = self.get_view_by_path(path)
        if not view:
            return
        changed = req['changed']
        begin = req['begin']
        old_length = req['old_length']
        self.emacs_bufs[path][0] = "%s%s%s" % (self.emacs_bufs[path][0][:begin - 1], changed, self.emacs_bufs[path][0][begin - 1 + old_length:])
        self.bufs_changed.append(view.buf['id'])

    def _on_highlight(self, req):
        view = self.get_view_by_path(req['full_path'])
        if view:
            self.selection_changed.append((view, req.get('ping', False)))

    def _on_create_buf(self, req):
        self.create_buf(req['full_path'])

    def _on_delete_buf(self, req):
        buf = self.get_buf_by_path(req['path'])
        if not buf:
            msg.debug('No buffer for path %s' % req['path'])
            return
        msg.log('deleting buffer ', buf['path'])
        event = {
            'name': 'delete_buf',
            'id': buf['id'],
        }
        self.agent.send(event)

    def _on_rename_buf(self, buf_id, new_path):
        new_path = utils.to_rel_path(new_path)
        if not utils.is_shared(new_path):
            msg.log('New path %s is not shared. Discarding rename event.' % new_path)
            return

        self.send_to_floobits({
            'name': 'rename_buf',
            'id': buf_id,
            'path': new_path,
        })
        # KANS: is this right? old code...
        old_path = self.agent.bufs[buf_id]['path']
        del self.agent.paths_to_ids[old_path]
        self.agent.paths_to_ids[new_path] = buf_id
        self.agent.bufs[buf_id]['path'] = new_path

    def _on_buffer_list_change(self, req):
        added = req.get('added') or {}
        for path, text in added.iteritems():
            buf = self.get_buf_by_path(path)
            self.emacs_bufs[path][0] = text
            if not buf:
                msg.debug('no buf for path %s' % path)
                self.create_buf(path, text=text)
                continue
            view = self.views.get(buf['id'])
            if view is None:
                self.get_view(buf['id'])
            elif view.is_loading():
                view._emacs_buf = self.emacs_bufs[path]
            else:
                msg.debug('view for buf %s already exists. this is not good. we got out of sync' % buf['path'])

        deleted = req.get('deleted') or []
        for path in deleted:
            if self.emacs_bufs.get(path) is None:
                msg.debug('emacs deleted %s but we already deleted it from emacs_bufs' % path)
            del self.emacs_bufs[path]
            buf = self.get_buf_by_path(path)
            if buf:
                del self.views[buf['id']]

        seen = set()
        current = req.get('current') or []
        for path in current:
            if self.emacs_bufs.get(path) is None:
                msg.debug('We should have buffer %s in emacs_bufs but we don\'t' % path)
            else:
                seen.add(path)

        for buf_id, view in self.views.iteritems():
            if utils.get_full_path(view.buf['path']) not in seen:
                msg.debug('We should not have buffer %s in our views but we do.' % view.buf['path'])

    def _on_saved(self, req):
        buf = self.get_buf_by_path(req['path'])
        if not buf:
            msg.debug('No buffer for path %s' % req['path'])
            return
        event = {
            'name': 'saved',
            'id': buf['id'],
        }
        self.agent.send(event)

    def _on_open_workspace(self, req):
        try:
            webbrowser.open(self.agent.workspace_url, new=2, autoraise=True)
        except Exception as e:
            msg.error("Couldn't open a browser: %s" % (str(e)))

    def _on_open_workspace_settings(self, req):
        try:
            webbrowser.open(self.agent.workspace_url + '/settings', new=2, autoraise=True)
        except Exception as e:
            msg.error("Couldn't open a browser: %s" % (str(e)))

    def _on_share_dir(self, data):
        utils.reload_settings()
        G.USERNAME = data['username']
        G.SECRET = data['secret']
        dir_to_share = data['dir_to_share']
        perms = data.get('perms')
        dir_to_share = os.path.expanduser(dir_to_share)
        dir_to_share = utils.unfuck_path(dir_to_share)
        workspace_name = os.path.basename(dir_to_share)
        G.PROJECT_PATH = os.path.realpath(dir_to_share)
        msg.debug('%s %s %s' % (G.USERNAME, workspace_name, G.PROJECT_PATH))

        if os.path.isfile(dir_to_share):
            return msg.error('%s is a file. Give me a directory please.' % dir_to_share)

        try:
            utils.mkdir(dir_to_share)
        except Exception:
            return msg.error("The directory %s doesn't exist and I can't make it." % dir_to_share)

        floo_file = os.path.join(dir_to_share, '.floo')

        info = {}
        try:
            floo_info = open(floo_file, 'rb').read().decode('utf-8')
            info = json.loads(floo_info)
        except (IOError, OSError):
            pass
        except Exception:
            msg.debug("Couldn't read the floo_info file: %s" % floo_file)

        workspace_url = info.get('url')
        if workspace_url:
            try:
                result = utils.parse_url(workspace_url)
            except Exception as e:
                msg.error(str(e))
            else:
                workspace_name = result['workspace']
                try:
                    # TODO: blocking. beachballs sublime 2 if API is super slow
                    api.get_workspace_by_url(workspace_url)
                except HTTPError:
                    workspace_url = None
                    workspace_name = os.path.basename(dir_to_share)
                else:
                    utils.add_workspace_to_persistent_json(result['owner'], result['workspace'], workspace_url, dir_to_share)

        workspace_url = utils.get_workspace_by_path(dir_to_share) or workspace_url

        if workspace_url:
            try:
                api.get_workspace_by_url(workspace_url)
            except HTTPError:
                pass
            else:
                agent = self.remote_connect(result['owner'], result['workspace'], False)
                return agent.once("room_info", lambda: agent.upload(dir_to_share))

        def on_done(data, choices=None):
            self.create_workspace({}, workspace_name, dir_to_share, owner=data.get('response'), perms=perms)

        orgs = api.get_orgs_can_admin()
        orgs = json.loads(orgs.read().decode('utf-8'))
        if len(orgs) == 0:
            return on_done({'response': G.USERNAME})
        i = 0
        choices = []
        choices.append([G.USERNAME, i])
        for o in orgs:
            i += 1
            choices.append([o['name'], i])

        self.get_input('Create workspace for (%s) ' % " ".join([x[0] for x in choices]), '', on_done, choices=choices)

    def _on_create_workspace(self, data, workspace_name, dir_to_share, owner=None, perms=None):
        owner = owner or G.USERNAME
        workspace_name = data.get('response', workspace_name)
        prompt = 'workspace %s already exists. Choose another name: ' % workspace_name
        try:
            api_args = {
                'name': workspace_name,
                'owner': owner,
            }
            if perms:
                api_args['perms'] = perms
            api.create_workspace(api_args)
            workspace_url = utils.to_workspace_url({'secure': True, 'owner': owner, 'workspace': workspace_name})
            msg.debug('Created workspace %s' % workspace_url)
        except HTTPError as e:
            err_body = e.read()
            msg.error('Unable to create workspace: %s %s' % (unicode(e), err_body))
            if e.code not in [400, 402, 409]:
                return msg.error('Unable to create workspace: %s' % str(e))
            if e.code == 400:
                workspace_name = re.sub('[^A-Za-z0-9_\-]', '-', workspace_name)
                prompt = 'Invalid name. Workspace names must match the regex [A-Za-z0-9_\-]. Choose another name:'
            elif e.code == 402:
                try:
                    err_body = json.loads(err_body)
                    err_body = err_body['detail']
                except Exception:
                    pass
                return editor.error_message('%s' % err_body)
            else:
                prompt = 'Workspace %s/%s already exists. Choose another name:' % (owner, workspace_name)

            return self.get_input(prompt, workspace_name, self.create_workspace, workspace_name, dir_to_share, owner, perms)
        except Exception as e:
            return msg.error('Unable to create workspace: %s' % str(e))

        G.PROJECT_PATH = dir_to_share
        agent = self.remote_connect(owner, workspace_name, False)
        agent.once("room_info", lambda: agent.upload(dir_to_share))

    def join_workspace(self, data, owner, workspace, dir_to_make=None):
        d = data['response']
        if dir_to_make:
            if d:
                d = dir_to_make
                utils.mkdir(d)
            else:
                d = ''
        if d == '':
            return self.get_input('Give me a directory to sync data to: ', G.PROJECT_PATH, self.join_workspace, owner, workspace)
        d = os.path.realpath(os.path.expanduser(d))
        if not os.path.isdir(d):
            if dir_to_make:
                return msg.error("Couldn't create directory %s" % dir_to_make)
            prompt = '%s is not a directory. Create it? ' % d
            return self.get_input(prompt, '', self.join_workspace, owner, workspace, dir_to_make=d, y_or_n=True)
        try:
            G.PROJECT_PATH = d
            utils.mkdir(os.path.dirname(G.PROJECT_PATH))
            self.remote_connect(owner, workspace)
        except Exception as e:
            return msg.error("Couldn't create directory %s: %s" % (G.PROJECT_PATH, str(e)))

    def _on_join_workspace(self, data):
        workspace = data['workspace']
        owner = data['workspace_owner']
        G.USERNAME = data['username']
        G.SECRET = data['secret']
        utils.reload_settings()
        try:
            G.PROJECT_PATH = utils.get_persistent_data()['workspaces'][owner][workspace]['path']
        except Exception:
            G.PROJECT_PATH = ''

        if G.PROJECT_PATH and os.path.isdir(G.PROJECT_PATH):
            return self.remote_connect(owner, workspace)

        G.PROJECT_PATH = '~/floobits/share/%s/%s' % (owner, workspace)
        self.get_input('Give me a directory to sync data to: ', G.PROJECT_PATH, self.join_workspace, owner, workspace)
