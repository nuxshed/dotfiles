local bling = require "modules.bling"

Scratch = {}

Scratch.term = bling.module.scratchpad {
  command = "alacritty --class=scratch",
  rule = { instance = "scratch" },
  sticky = true,
  autoclose = false,
  floating = true,
  geometry = { x = 830, y = 440, height = 300, width = 500 },
  reapply = true,
  dont_focus_before_close = false,
}
