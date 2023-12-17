# evil-wezterm-navigator

For the Ctrl-{h,j,k,l} folks who used to use [vim-tmux-navigator](https://github.com/christoomey/vim-tmux-navigator) or similar...

This package aims to replace tmux workflow for [WezTerm](https://github.com/wez/wezterm) users. Since `wezterm` supports [multiplexing](https://wezfurlong.org/wezterm/multiplexing.html)
natively, it seems unnecessary to use [Tmux](https://github.com/tmux/tmux) instead. The only missing part is a dedicated plugin
which allows switching between editor's windows and `wezterm` panes seamlessly.

# Dependencies

* Emacs
- evil

# Installation and configuration

Via straight.el:

```lisp
(straight-use-package
   '(navigate :type git :host codeberg :repo "eli87/evil-wezterm-navigator"))
```

Then require it in your `~/.emacs` with:

```lisp
(require 'navigate)
```

You also have to setup commands in your `wezterm.lua`:

```lua
local wezterm = require 'wezterm'

local config = {}

if wezterm.config_builder then
   config = wezterm.config_builder()
end

-- Tmux like Ctrl-{h,j,k,l} navigation.

local code_to_escape_sequence = {
   ["h"] = "\x08",
   ["j"] = "\x0a",
   ["k"] = "\x0b",
   ["l"] = "\x0c",
}

local editor_prefix_title = "emacs"

local move_around = function(window, pane, direction_wez, direction_nvim)
   if pane:get_title():sub(-string.len(editor_prefix_title)) == editor_prefix_title then
	  sequence = code_to_escape_sequence[direction_nvim]
	  window:perform_action(wezterm.action{SendString=sequence}, pane)
   else
	  window:perform_action(wezterm.action{ActivatePaneDirection=direction_wez}, pane)
   end
end

wezterm.on("move-left", function(window, pane) move_around(window, pane, "Left", "h") end)
wezterm.on("move-right", function(window, pane) move_around(window, pane, "Right", "l") end)
wezterm.on("move-up", function(window, pane) move_around(window, pane, "Up", "k") end)
wezterm.on("move-down", function(window, pane) move_around(window, pane, "Down", "j") end)

-- Use Ctrl+Space as a prefix key.
config.leader = { key="Space", mods="CTRL" }
config.keys = {
   { key = "Space", mods = "LEADER|CTRL",  action=wezterm.action{SendString="\x01"}},
   { key = "\\",mods = "LEADER",           action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
   { key = "\"", mods = "LEADER|SHIFT",    action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
   { key = "%", mods = "LEADER|SHIFT",     action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
   { key = "z", mods = "LEADER",           action="TogglePaneZoomState" },
   { key = "c", mods = "LEADER",           action=wezterm.action{SpawnTab="CurrentPaneDomain"}},
   { key = "h", mods = "CTRL",             action=wezterm.action{EmitEvent="move-left"}},
   { key = "j", mods = "CTRL",             action=wezterm.action{EmitEvent="move-down"}},
   { key = "k", mods = "CTRL",             action=wezterm.action{EmitEvent="move-up"}},
   { key = "l", mods = "CTRL",             action=wezterm.action{EmitEvent="move-right"}},
   { key = "H", mods = "LEADER|SHIFT",     action=wezterm.action{AdjustPaneSize={"Left", 5}}},
   { key = "J", mods = "LEADER|SHIFT",     action=wezterm.action{AdjustPaneSize={"Down", 5}}},
   { key = "K", mods = "LEADER|SHIFT",     action=wezterm.action{AdjustPaneSize={"Up", 5}}},
   { key = "L", mods = "LEADER|SHIFT",     action=wezterm.action{AdjustPaneSize={"Right", 5}}},
   { key = "1", mods = "LEADER",           action=wezterm.action{ActivateTab=0}},
   { key = "2", mods = "LEADER",           action=wezterm.action{ActivateTab=1}},
   { key = "3", mods = "LEADER",           action=wezterm.action{ActivateTab=2}},
   { key = "4", mods = "LEADER",           action=wezterm.action{ActivateTab=3}},
   { key = "5", mods = "LEADER",           action=wezterm.action{ActivateTab=4}},
   { key = "6", mods = "LEADER",           action=wezterm.action{ActivateTab=5}},
   { key = "7", mods = "LEADER",           action=wezterm.action{ActivateTab=6}},
   { key = "8", mods = "LEADER",           action=wezterm.action{ActivateTab=7}},
   { key = "9", mods = "LEADER",           action=wezterm.action{ActivateTab=8}},
   { key = "x", mods = "LEADER",           action=wezterm.action{CloseCurrentPane={confirm=false}}},
   { key = "[", mods="LEADER",             action="ActivateCopyMode"},
}

return config
```

# References

* https://github.com/wez/wezterm
* https://github.com/keith/evil-tmux-navigator
* https://github.com/wez/wezterm/discussions/995
* https://github.com/aca/wezterm.nvim
* https://github.com/christoomey/vim-tmux-navigator
