#!/usr/bin/env python

# coding: utf-8
import os
import json
import urllib2
import webbrowser
import select
import socket
import sys

from floo import AgentConnection
from floo.common import api
from floo.common import msg
from floo.common import shared as G
from floo.common import utils
from floo.emacs_protocol import Protocol


utils.reload_settings()

# enable debug with let floo_log_level = 'debug'
floo_log_level = 'debug'
msg.LOG_LEVEL = msg.LOG_LEVELS.get(floo_log_level.upper(), msg.LOG_LEVELS['MSG'])

agent = None


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

    def get_input(self, prompt, initial, cb, *args, **kwargs):
        self.put('user_input', {
            'id': self.user_input_count,
            'prompt': prompt,
            'initial': initial,
        })
        self.user_inputs[self.user_input_count] = lambda x: cb(x, *args, **kwargs)
        self.user_input_count += 1

    def start(self):
        print('started')
        self.conn, addr = self.sock.accept()
        self.conn.setblocking(0)
        self.select()

    def remote_connect(self, owner, room, on_auth=None):
        G.PROJECT_PATH = os.path.realpath(G.PROJECT_PATH)
        G.PROJECT_PATH += os.sep
        self.agent = AgentConnection(Protocol=Protocol, room=room, owner=owner, on_auth=on_auth)
        self.agent.connect()

    def share_dir(self, dir_to_share):
        dir_to_share = os.path.expanduser(dir_to_share)
        dir_to_share = utils.unfuck_path(dir_to_share)
        room_name = os.path.basename(dir_to_share)
        floo_room_dir = os.path.join(G.COLAB_DIR, G.USERNAME, room_name)
        G.PROJECT_PATH = os.path.realpath(floo_room_dir)
        msg.debug("%s %s %s %s" % (G.COLAB_DIR, G.USERNAME, room_name, floo_room_dir))

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

        room_url = info.get('url')
        if room_url:
            try:
                result = utils.parse_url(room_url)
            except Exception as e:
                msg.error(str(e))
            else:
                room_name = result['room']
                floo_room_dir = os.path.join(G.COLAB_DIR, result['owner'], result['room'])
                if os.path.realpath(floo_room_dir) == os.path.realpath(dir_to_share):
                    if result['owner'] == G.USERNAME:
                        try:
                            api.create_room(room_name)
                            msg.debug('Created room %s' % room_url)
                        except Exception as e:
                            msg.debug('Tried to create room' + str(e))
                    # they wanted to share teh dir, so always share it
                    G.PROJECT_PATH = os.path.realpath(floo_room_dir)
                    return self.remote_connect(result['owner'], result['room'])
        # go make sym link
        try:
            utils.mkdir(os.path.dirname(floo_room_dir))
            os.symlink(dir_to_share, floo_room_dir)
        except OSError as e:
            if e.errno != 17:
                raise
        except Exception as e:
            return msg.error("Couldn't create symlink from %s to %s: %s" % (dir_to_share, floo_room_dir, str(e)))

        # make & join room
        self.create_room({}, room_name, floo_room_dir, dir_to_share)

    def create_room(self, data, room_name, ln_path, share_path):
        new_room_name = data.get('response')
        if new_room_name:
            room_name = new_room_name
        prompt = 'Room %s already exists. Choose another name: ' % room_name
        try:
            api.create_room(room_name)
            room_url = 'https://%s/r/%s/%s' % (G.DEFAULT_HOST, G.USERNAME, room_name)
            msg.debug('Created room %s' % room_url)

            if new_room_name:
                new_path = os.path.join(os.path.dirname(ln_path), room_name)
                try:
                    os.rename(ln_path, new_path)
                except OSError:
                    initial = room_name + '1'
                    return self.get_input(prompt, initial, self.create_room, room_name, new_path, share_path)

        except urllib2.HTTPError as e:
            if e.code != 409:
                raise
            initial = room_name + '1'
            return self.get_input(prompt, initial, self.create_room, room_name, ln_path, share_path)
        except Exception as e:
            return msg.error('Unable to create room: %s' % str(e))

        try:
            webbrowser.open(room_url + '/settings', new=2, autoraise=True)
        except Exception:
            msg.debug("Couldn't open a browser. Thats OK!")
        G.PROJECT_PATH = share_path
        self.remote_connect(G.USERNAME, room_name, lambda this: this.protocol.create_buf(share_path))

    def join_room(self, data, owner, room, dir_to_make=None):
        d = data['response']
        if dir_to_make:
            if d.lower() == 'y':
                d = dir_to_make
                utils.mkdir(d)
            else:
                d = ''
        if d == '':
            utils.mkdir(G.PROJECT_PATH)
            self.remote_connect(owner, room)
            return
        d = os.path.realpath(os.path.expanduser(d))
        if not os.path.isdir(d):
            if dir_to_make:
                return msg.error("Couldn't create directory %s" % dir_to_make)
            prompt = '%s is not a directory. Create it? (Y/N)' % d
            return self.get_input(prompt, '', self.join_room, owner, room, dir_to_make=d)
        try:
            G.PROJECT_PATH = os.path.realpath(G.PROJECT_PATH)
            utils.mkdir(os.path.dirname(G.PROJECT_PATH))
            os.symlink(d, G.PROJECT_PATH)
            self.remote_connect(owner, room)
        except Exception as e:
            return msg.error("Couldn't create symlink from %s to %s: %s" % (d, G.PROJECT_PATH, str(e)))

    def handle(self, req):
        self.net_buf += req
        while True:
            before, sep, after = self.net_buf.partition('\n')
            if not sep:
                break
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
                self.share_dir(data['dir_to_share'])
            elif data['name'] == 'join_room':
                utils.reload_settings()
                room = data['room']
                owner = data['room_owner']
                G.USERNAME = data['username']
                G.SECRET = data['secret']
                G.PROJECT_PATH = os.path.realpath(os.path.join(G.COLAB_DIR, owner, room))

                if os.path.isdir(G.PROJECT_PATH):
                    self.remote_connect(owner, room)
                else:
                    self.get_input('Give me a directory to sync data into (or just press enter): ', '', self.join_room, owner, room)
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
            self.net_buf = after

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
                        msg.debug("to emacs: %s" % p)
                        self.conn.sendall(p)
                    except Exception as e:
                        msg.error('Couldn\'t write to socket: %s' % str(e))
                        return self.reconnect()


if __name__ == '__main__':
    G.EMACS = EmacsConnection()
    G.EMACS.start()
