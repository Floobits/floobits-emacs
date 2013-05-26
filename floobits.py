#!/usr/bin/env python

# coding: utf-8
import os
import json
import traceback
import urllib2
import atexit
import webbrowser
import select
import socket
import subprocess
import sys

from floo import dmp_monkey
dmp_monkey.monkey_patch()

from floo import sublime
from floo import AgentConnection
from floo import msg
from floo import shared as G
from floo import utils
from floo import api
from floo.emacs_protocol import Protocol


utils.load_settings()

# enable debug with let floo_log_level = 'debug'
floo_log_level = 'debug'
msg.LOG_LEVEL = msg.LOG_LEVELS.get(floo_log_level.upper(), msg.LOG_LEVELS['MSG'])

agent = None


# def agent_and_protocol(func):
#     def wrapped(*args, **kwargs):
#         if agent and agent.protocol:
#             return func(*args, **kwargs)
#         msg.debug('ignoring request becuase there is no agent: %s' % func.__name__)
#     return wrapped


# @agent_and_protocol
# def maybe_selection_changed(ping=False):
#     agent.protocol.maybe_selection_changed(vim.current.buffer, ping)


# @agent_and_protocol
# def maybe_buffer_changed():
#     agent.protocol.maybe_buffer_changed(vim.current.buffer)


# @agent_and_protocol
# def follow(follow_mode=None):
#     agent.protocol.follow(follow_mode)


# def is_modifiable(name_to_check=None):
#     if not agent or not agent.protocol:
#         return
#     vim_buf = vim.current.buffer
#     name = vim_buf.name
#     if not name:
#         return
#     if name_to_check and name_to_check != name:
#         msg.warn('Can not call readonly on file: %s' % name)
#     if not agent.protocol.is_shared(name):
#         return
#     if 'patch' not in agent.protocol.perms:
#         vim.command("call g:FlooSetReadOnly()")
#         sublime.set_timeout(is_modifiable, 0, name)


# @agent_and_protocol
# def maybe_new_file():
#     vim_buf = vim.current.buffer
#     buf = agent.protocol.get_buf(vim_buf)
#     if buf is False:
#         agent.protocol.create_buf(vim_buf.name)


def share_dir(dir_to_share):
    dir_to_share = os.path.expanduser(dir_to_share)
    dir_to_share = utils.unfuck_path(dir_to_share)
    room_name = os.path.basename(dir_to_share)
    floo_room_dir = os.path.join(G.COLAB_DIR, G.USERNAME, room_name)

    if os.path.isfile(dir_to_share):
        return msg.error('give me a directory please')

    if not os.path.isdir(dir_to_share):
        return msg.error('The directory %s doesn\'t appear to exist' % dir_to_share)

    floo_file = os.path.join(dir_to_share, '.floo')
    # look for the .floo file for hints about previous behavior
    info = {}
    try:
        floo_info = open(floo_file, 'rb').read().decode('utf-8')
        info = json.loads(floo_info)
    except (IOError, OSError):
        pass
    except Exception:
        msg.warn("couldn't read the floo_info file: %s" % floo_file)

    room_url = info.get('url')
    if room_url:
        try:
            result = utils.parse_url(room_url)
        except Exception as e:
            msg.error(str(e))
        else:
            room_name = result['room']
            floo_room_dir = os.path.join(G.COLAB_DIR, result['owner'], result['room'])
            # they have previously joined the room
            if os.path.realpath(floo_room_dir) == os.path.realpath(dir_to_share):
                # it could have been deleted, try to recreate it if possible
                # TODO: org or something here?
                if result['owner'] == G.USERNAME:
                    try:
                        api.create_room(room_name)
                        msg.debug('Created room %s' % room_url)
                    except Exception as e:
                        msg.debug('Tried to create room' + str(e))
                # they wanted to share teh dir, so always share it
                return join_room(room_url, lambda x: agent.protocol.create_buf(dir_to_share))

    # link to what they want to share
    try:
        utils.mkdir(os.path.dirname(floo_room_dir))
        os.symlink(dir_to_share, floo_room_dir)
    except OSError as e:
        if e.errno != 17:
            raise
    except Exception as e:
        return msg.error("Couldn't create symlink from %s to %s: %s" % (dir_to_share, floo_room_dir, str(e)))

    # make & join room
    create_room(room_name, floo_room_dir, dir_to_share)


def create_room(room_name, ln_path=None, share_path=None):
    try:
        api.create_room(room_name)
        room_url = 'https://%s/r/%s/%s' % (G.DEFAULT_HOST, G.USERNAME, room_name)
        msg.debug('Created room %s' % room_url)
    except urllib2.HTTPError as e:
        if e.code != 409:
            raise
        if ln_path:
            while True:
                room_name = 'whatever'  # vim_input('Room %s already exists. Choose another name: ' % room_name, room_name + "1")
                new_path = os.path.join(os.path.dirname(ln_path), room_name)
                try:
                    os.rename(ln_path, new_path)
                except OSError:
                    continue
                ln_path = new_path
                break

        return create_room(room_name, ln_path, share_path)
    except Exception as e:
        sublime.error_message('Unable to create room: %s' % str(e))
        return

    try:
        webbrowser.open(room_url + '/settings', new=2, autoraise=True)
    except Exception:
        msg.debug("Couldn't open a browser. Thats OK!")
    join_room(room_url, lambda x: agent.protocol.create_buf(share_path))


# @agent_and_protocol
# def delete_buf():
#     name = vim.current.buffer.name
#     agent.protocol.delete_buf(name)


# def join_room(room_url, on_auth=None):
#     global agent
#     msg.debug("room url is %s" % room_url)

#     try:
#         result = utils.parse_url(room_url)
#     except Exception as e:
#         return msg.error(str(e))

#     G.PROJECT_PATH = os.path.realpath(os.path.join(G.COLAB_DIR, result['owner'], result['room']))
#     utils.mkdir(os.path.dirname(G.PROJECT_PATH))

#     d = ''
#     # TODO: really bad prompt here
#     prompt = "Give me a directory to destructively dump data into (or just press enter): "
#     if not os.path.isdir(G.PROJECT_PATH):
#         while True:
#             d = vim_input(prompt, d)
#             if d == '':
#                 utils.mkdir(G.PROJECT_PATH)
#                 break
#             d = os.path.realpath(os.path.expanduser(d))
#             if not os.path.isdir(d):
#                 prompt = '%s is not a directory. Enter an existing path (or press enter): ' % d
#                 continue
#             try:
#                 os.symlink(d, G.PROJECT_PATH)
#                 break
#             except Exception as e:
#                 return msg.error("Couldn't create symlink from %s to %s: %s" % (d, G.PROJECT_PATH, str(e)))

#     vim.command('cd %s' % G.PROJECT_PATH)
#     msg.debug("joining room %s" % room_url)

#     stop_everything()
#     try:
#         start_event_loop()
#         agent = AgentConnection(on_auth=on_auth, Protocol=Protocol, **result)
#         # owner and room name are slugfields so this should be safe
#         agent.connect()
#     except Exception as e:
#         msg.error(str(e))
#         tb = traceback.format_exc()
#         msg.debug(tb)
#         stop_everything()


# def part_room():
#     if not agent:
#         return msg.warn('Unable to part room: You are not joined to a room.')
#     stop_everything()
#     msg.log('You left the room.')


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

    def get_input(self, prompt, cb, *args, **kwargs):
        self.put('user_input', {
            'id': self.user_input_count,
            'prompt': prompt,
        })
        self.user_inputs[self.user_input_count] = lambda x: cb(x, *args, **kwargs)
        self.user_input_count += 1

    def start(self):
        print('started')
        self.conn, addr = self.sock.accept()
        self.conn.setblocking(0)
        self.select()

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
            if data['name'] == 'auth':
                utils.load_settings()
                room = data['room']
                owner = data['room_owner']
                G.USERNAME = data['username']
                G.SECRET = data['secret']
                G.PROJECT_PATH = os.path.realpath(os.path.join(G.COLAB_DIR, owner, room))
                G.PROJECT_PATH += os.sep

                # TODO: MEGA CLEANUP
                def handle_input(data, dir_to_make=None):
                    d = data['response']
                    if dir_to_make:
                        if d.lower() == 'y':
                            d = dir_to_make
                            utils.mkdir(d)
                        else:
                            d = ''
                    if d == '':
                        utils.mkdir(G.PROJECT_PATH)
                        G.PROJECT_PATH = os.path.realpath(G.PROJECT_PATH)
                        G.PROJECT_PATH += os.sep
                        self.agent = AgentConnection(Protocol=Protocol, room=room, owner=owner)
                        self.agent.connect()
                        return
                    d = os.path.realpath(os.path.expanduser(d))
                    if not os.path.isdir(d):
                        if dir_to_make:
                            return msg.error("Couldn't create directory %s" % dir_to_make)
                        prompt = '%s is not a directory. Create it? (Y/N)' % d
                        return self.get_input(prompt, handle_input, dir_to_make=d)
                    try:
                        G.PROJECT_PATH = os.path.realpath(G.PROJECT_PATH)
                        os.symlink(d, G.PROJECT_PATH)
                        G.PROJECT_PATH += os.sep
                        self.agent = AgentConnection(Protocol=Protocol, room=room, owner=owner)
                        self.agent.connect()
                    except Exception as e:
                        return msg.error("Couldn't create symlink from %s to %s: %s" % (d, G.PROJECT_PATH, str(e)))
                if os.path.isdir(G.PROJECT_PATH):
                    G.PROJECT_PATH = os.path.realpath(G.PROJECT_PATH)
                    G.PROJECT_PATH += os.sep
                    self.agent = AgentConnection(Protocol=Protocol, room=room, owner=owner)
                    self.agent.connect()
                else:
                    self.get_input("Give me a directory to sync data into (or just press enter): ", handle_input)
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
