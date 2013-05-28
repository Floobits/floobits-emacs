# [Floobits](https://floobits.com/) plugin for Emacs

Real-time collaborative editing. Think Etherpad, but with native editors. This is the plugin for Emacs. We also have plugins for [Sublime Text](https://github.com/Floobits/sublime-text-2-plugin) and [Vim](https://github.com/Floobits/vim-plugin).

### Development status: new, but reasonably stable. We've tested it on Emacs 23, but earlier or later versions might work. The plugin requires Python 2.7.


## Initial set-up

* [Create a Floobits account](https://floobits.com/signup/) or [sign in with GitHub](https://floobits.com/login/github/?next=/dash/).
* `cd ~/.emacs.d/`
* `git clone https://github.com/Floobits/floobits-emacs.git floobits`

* Add Floobits to your `~/.emacs`: `(load "~/.emacs.d/floobits/floobits.el")`

* Add your Floobits username and API secret to `~/.floorc`. You can find your API secret on [your settings page](https://floobits.com/dash/settings/). A typical `~/.floorc` looks like this:

```
username myuser
secret gii9Ka8aZei3ej1eighu2vi8D
vim_executable mvim
```

You'll need to Restart Emacs once you've installed the plugin.


## Usage

All commands are documented in `apropos-command <RET> floobits`
<ul>
  <li><code>M-x floobits-join-room &lt;RET&gt; https://floobits.com/r/owner/room/ &lt;RET&gt;</code> &nbsp; Join an existing floobits room.</li>
  <li><code>M-x floobits-share-dir &lt;RET&gt; DIR &lt;RET&gt;</code> &nbsp; Create a room and populate it with the contents of the directory, DIR (or make it).</li>
  <li><code>M-x floobits-leave-room &lt;RET&gt;</code> &nbsp; Leave the current room.</li>
  <li><code>M-x floobits-summon &lt;RET&gt;</code> &nbsp; Summon everyone in the room to your cursor position.</li>
  <li><code>M-x floobits-follow-mode-toggle &lt;RET&gt;</code> &nbsp; Toggle following of recent changes.</li>
  <li><code>M-x floobits-clear-highlights &lt;RET&gt;</code> &nbsp; Clears all mirrored highlights.</li>
</ul>
