# evil-terminal-multiplexer

For the Ctrl-{h,j,k,l} folks who used to use
[vim-tmux-navigator](https://github.com/christoomey/vim-tmux-navigator) or
similar...

This package aims to envelop both [WezTerm](https://github.com/wez/wezterm) and
[Tmux](https://github.com/tmux/tmux) into the single navigate and resize workflow.

# Dependencies

- Emacs
- evil
- [WezTerm](https://github.com/wez/wezterm) or [Tmux](https://github.com/tmux/tmux)

# Installation and configuration

Via straight.el:

```lisp
(straight-use-package
   '(etm :type git :host github :repo "illia-danko/evil-terminal-multiplexer"))
```

Then require it in your `~/.emacs` with:

```lisp
(require 'etm)
```

# Tmux config

```
is_editor="ps -o state= -o comm= -t '#{pane_tty}' \
    | grep -iqE '^[^TXZ ]+ +(\\S+\\/)?g?(view|nvim)(diff)?$|emacs.*$'"

# Bindings.
bind -n C-h if-shell "$is_editor" "send-keys C-h" "select-pane -L"
bind -n C-l if-shell "$is_editor" "send-keys C-l" "select-pane -R"
bind -n C-j if-shell "$is_editor" "send-keys C-j" "select-pane -D"
bind -n C-k if-shell "$is_editor" "send-keys C-k" "select-pane -U"
```

# WezTerm config

```lua
-- NOTE: This config implies resizing panes usage. However this is not supported yet.

local wezterm = require("wezterm")

local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

local function is_editor(pane)
  local process_name = string.gsub(pane:get_foreground_process_name(), '(.*[/\\])(.*)', '%2')
  return process_name == 'nvim' or process_name == 'vim' or string.find(process_name, 'emacs')
end

local direction_keys = {
  h = 'Left',
  j = 'Down',
  k = 'Up',
  l = 'Right',
}

local function editor_nav_key(resize_or_move, key)
  return {
    key = key,
    mods = resize_or_move == 'resize' and 'META' or 'CTRL',
    action = wezterm.action_callback(function(win, pane)
      if is_editor(pane) then
        win:perform_action({
          SendKey = { key = key, mods = resize_or_move == 'resize' and 'META' or 'CTRL' },
        }, pane)
      else
        if resize_or_move == 'resize' then
          win:perform_action({ AdjustPaneSize = { direction_keys[key], 3 } }, pane)
        else
          win:perform_action({ ActivatePaneDirection = direction_keys[key] }, pane)
        end
      end
    end),
  }
end

config.keys = {
  editor_nav_key('move', 'h'),
  editor_nav_key('move', 'j'),
  editor_nav_key('move', 'k'),
  editor_nav_key('move', 'l'),

  ---- resize panes
  -- editor_nav_key('resize', 'h'),
  -- editor_nav_key('resize', 'j'),
  -- editor_nav_key('resize', 'k'),
  -- editor_nav_key('resize', 'l'),
}
```


# References

* https://github.com/wez/wezterm
* https://github.com/keith/evil-tmux-navigator
* https://github.com/wez/wezterm/discussions/995
* https://github.com/aca/wezterm.nvim
* https://github.com/christoomey/vim-tmux-navigator
* https://github.com/mrjones2014/smart-splits.nvim
