#!/usr/bin/env python

# coding: utf-8
import os
import json
import re
import select
import socket
import sys
from urllib2 import HTTPError

from floo import AgentConnection
from floo.common import api
from floo.common import migrations
from floo.common import msg
from floo.common import shared as G
from floo.common import utils
from floo import sublime
from floo.emacs_protocol import Protocol


G.__VERSION__ = '0.03'
G.__PLUGIN_VERSION__ = '0.2'
utils.reload_settings()

# enable debug with let floo_log_level = 'debug'
floo_log_level = 'debug'
msg.LOG_LEVEL = msg.LOG_LEVELS.get(floo_log_level.upper(), msg.LOG_LEVELS['MSG'])

agent = None

migrations.rename_floobits_dir()
migrations.migrate_symlinks()


class EmacsConnection(object):
    def __init__(self):
        self.to_emacs_q = []
        self.net_buf = ''
        self.agent = None

        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.sock.bind(('localhost', 4567))
        self.sock.listen(1)
        self.user_inputs = {}
        self.user_input_count = 0
        if sys.version_info[0] == 2 and sys.version_info[1] == 6:
            # Work around http://bugs.python.org/issue11326
            msg.error('Disabling SSL to work around a bug in Python 2.6. Please upgrade your Python to get SSL. See http://bugs.python.org/issue11326')
            G.SECURE = False
            G.DEFAULT_PORT = 3148

    def get_input(self, prompt, initial, cb, *args, **kwargs):
        event = {
            'id': self.user_input_count,
            'prompt': prompt,
            'initial': initial,
        }
        if 'choices' in kwargs:
            event['choices'] = kwargs['choices']
        elif 'y_or_n' in kwargs:
            event['y_or_n'] = True
            del kwargs['y_or_n']
        self.put('user_input', event)
        self.user_inputs[self.user_input_count] = lambda x: cb(x, *args, **kwargs)
        self.user_input_count += 1

    def start(self):
        # Emacs watches for this line before connecting.
        print('Now_listening')
        self.conn, addr = self.sock.accept()
        self.conn.setblocking(0)
        self.select()

    def remote_connect(self, workspace_url, on_auth=None, get_bufs=True):
        G.PROJECT_PATH = os.path.realpath(G.PROJECT_PATH)
        G.PROJECT_PATH += os.sep
        self.agent = AgentConnection(Protocol=Protocol,
                                     workspace_url=workspace_url,
                                     on_auth=on_auth,
                                     get_bufs=get_bufs,
                                     conn=self)
        self.agent.connect()

    def share_dir(self, dir_to_share, perms=None):
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
                return self.remote_connect(workspace_url, on_auth=lambda this: this.protocol.create_buf(dir_to_share), get_bufs=False)

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

    def create_workspace(self, data, workspace_name, dir_to_share, owner=None, perms=None):
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
                return sublime.error_message('%s' % err_body)
            else:
                prompt = 'Workspace %s/%s already exists. Choose another name:' % (owner, workspace_name)

            return self.get_input(prompt, workspace_name, self.create_workspace, workspace_name, dir_to_share, owner, perms)
        except Exception as e:
            return msg.error('Unable to create workspace: %s' % str(e))

        G.PROJECT_PATH = dir_to_share
        self.remote_connect(workspace_url, on_auth=lambda this: this.protocol.create_buf(dir_to_share), get_bufs=False)

    def join_workspace(self, data, owner, workspace, dir_to_make=None):
        d = data['response']
        workspace_url = utils.to_workspace_url({'secure': True, 'owner': owner, 'workspace': workspace})
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
            self.remote_connect(workspace_url)
        except Exception as e:
            return msg.error("Couldn't create directory %s: %s" % (G.PROJECT_PATH, str(e)))

    def handle(self, req):
        self.net_buf += req
        while True:
            before, sep, after = self.net_buf.partition('\n')
            if not sep:
                break
            self.net_buf = after
            try:
                data = json.loads(before)
            except Exception as e:
                msg.error('Unable to parse json:', e)
                msg.error('Data:', before)
                raise e
            if data['name'] == 'share_dir':
                utils.reload_settings()
                G.USERNAME = data['username']
                G.SECRET = data['secret']
                self.share_dir(data['dir_to_share'], data.get('perms'))
            elif data['name'] == 'join_workspace':
                utils.reload_settings()
                workspace = data['workspace']
                owner = data['workspace_owner']
                G.USERNAME = data['username']
                G.SECRET = data['secret']

                try:
                    G.PROJECT_PATH = utils.get_persistent_data()['workspaces'][owner][workspace]['path']
                except Exception:
                    G.PROJECT_PATH = ''

                if G.PROJECT_PATH and os.path.isdir(G.PROJECT_PATH):
                    workspace_url = utils.to_workspace_url({'secure': True, 'owner': owner, 'workspace': workspace})
                    self.remote_connect(workspace_url)
                    continue

                G.PROJECT_PATH = '~/floobits/share/%s/%s' % (owner, workspace)
                self.get_input('Give me a directory to sync data to: ', G.PROJECT_PATH, self.join_workspace, owner, workspace)

            elif data['name'] == 'user_input':
                cb_id = int(data['id'])
                cb = self.user_inputs.get(cb_id)
                if cb is None:
                    msg.error('cb for input %s is none' % cb_id)
                    continue
                cb(data)
                del self.user_inputs[cb_id]
            else:
                self.agent.protocol.emacs_handle(data)

    def put(self, name, data):
        data['name'] = name
        self.to_emacs_q.append(json.dumps(data) + '\n')

    def reconnect(self):
        try:
            self.conn.shutdown(socket.SHUT_RDWR)
            self.conn.close()
        except Exception:
            pass
        self.sock.close()
        sys.exit(1)

    def select(self):
        if not self.conn:
            msg.error('select(): No socket.')
            return self.reconnect()

        while True:
            if self.agent:
                self.agent.tick()

            out_conns = []
            if len(self.to_emacs_q) > 0:
                out_conns.append(self.conn)

            try:
                _in, _out, _except = select.select([self.conn], out_conns, [self.conn], 0.05)
            except (select.error, socket.error, Exception) as e:
                msg.error('Error in select(): %s' % str(e))
                return self.reconnect()

            if _except:
                msg.error('Socket error')
                return self.reconnect()

            if _in:
                buf = ''
                while True:
                    try:
                        d = self.conn.recv(4096)
                        if not d:
                            break
                        buf += d
                    except (socket.error, TypeError):
                        break
                if buf:
                    self.empty_selects = 0
                    self.handle(buf)
                else:
                    self.empty_selects += 1
                    if self.empty_selects > 10:
                        msg.error('No data from sock.recv() {0} times.'.format(self.empty_selects))
                        return self.reconnect()

            if _out:
                while len(self.to_emacs_q) > 0:
                    p = self.to_emacs_q.pop(0)
                    try:
                        msg.debug('to emacs: %s' % p)
                        self.conn.sendall(p)
                    except Exception as e:
                        msg.error('Couldn\'t write to socket: %s' % str(e))
                        return self.reconnect()


if __name__ == '__main__':
    G.EMACS = EmacsConnection()
    G.EMACS.start()
