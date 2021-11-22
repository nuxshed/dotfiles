local awful = require "awful"
local beautiful = require "beautiful"
local naughty = require "naughty"

naughty.config.defaults.ontop = true
naughty.config.defaults.screen = awful.screen.focused()
naughty.config.defaults.timeout = 5
naughty.config.defaults.title = "Notification"
naughty.config.defaults.position = "top_right"

naughty.config.presets.normal = {
  font = beautiful.font,
  fg = "#c678dd",
  bg = beautiful.bg_normal,
}

naughty.config.presets.low = {
  font = beautiful.font,
  fg = beautiful.fg_normal,
  bg = beautiful.bg_normal,
}


