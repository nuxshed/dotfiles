C = {}

C.terminal = "alacritty"
C.browser = "firefox"
C.editor = "emacs"
C.notes = "obsidian"
C.modkey = "Mod4"
C.altmod = "Mod1"

require "conf.scratch"
require "conf.bound"
require "conf.client"
require "conf.layout"
require "conf.ruled"
require "conf.undementia"

local keyseq = require "conf.keyseq"
local awful = require "awful"

local seqs = {
  ["1 2 3"] = function()
    awful.spawn "alacritty"
  end,
  ["Tab 2 3"] = function()
    awful.spawn "emacs"
  end,
  ["a b c d e f g h"] = function()
    awful.spawn "mpv --loop /home/nuxsh/zzz.gif"
  end,
  ["q w e r t y u i o p"] = function()
    awful.spawn "mpv --loop /home/nuxsh/zzz2.gif"
  end,
  ["9 1 1"] = function()
    awful.spawn "mpv --loop /home/nuxsh/911.gif"
  end,
}

awful.keyboard.append_global_keybindings {
  awful.key({ "Mod4", "Shift" }, "s", function()
    keyseq.start(seqs)
  end),
}
