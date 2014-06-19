# coding: utf-8

try:
    from . import agent_connection
    from .common import flooui
except (ImportError, ValueError):
    import agent_connection
    from floo.common import flooui


class Emui(flooui.FlooUI):
    def __init__(self, emacs):
        super(Emui, self).__init__()
        self.emacs = emacs
        self.user_inputs = {}
        self.user_input_count = 0

    def _make_agent(self, context, owner, workspace, auth, created_workspace, d):
        """@returns new Agent()"""
        return agent_connection.AgentConnection(owner, workspace, context, auth, created_workspace and d)

    def user_y_or_n(self, context, prompt, affirmation_txt, cb):
        """@returns True/False"""
        self.user_input_count += 1
        event = {
            'name': 'user_input',
            'id': self.user_input_count,
            'prompt': prompt.replace('\n', ', ').replace(", ,", "") + '? ',
            'initial': "",
            'y_or_n': True
        }
        self.user_inputs[self.user_input_count] = cb
        context.send(event)

    def user_select(self, context, prompt, choices_big, choices_small, cb):
        """@returns (choice, index)"""
        self.user_input_count += 1
        choices = [["%d. %s" % (i + 1, v), i] for i, v in enumerate(choices_big)]
        event = {
            'name': 'user_input',
            'id': self.user_input_count,
            'prompt': prompt + "\n\n%s\n\nPlease select an option: " % "\n".join([c for c in choices_big]),
            'initial': "",
            'choices': choices
        }
        self.user_inputs[self.user_input_count] = lambda choice: cb(choice[3:], )
        context.send(event)

    def user_charfield(self, context, prompt, initial, cb):
        """@returns String"""
        self.user_input_count += 1
        event = {
            'name': 'user_input',
            'id': self.user_input_count,
            'prompt': prompt,
            'initial': initial,
        }
        self.user_inputs[self.user_input_count] = cb
        context.send(event)

    def get_a_window(self, abs_path, cb):
        """opens a project in a window or something"""
        return cb()

    def on_user_input(self, cb_id, res):
        cb = self.user_inputs.get(cb_id)
        if cb is None:
            print('cb for input %s is none' % cb_id)
            return
        cb(res)
        del self.user_inputs[cb_id]
