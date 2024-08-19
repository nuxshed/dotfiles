require "awful.autofocus"

_G.theme = "bberry"

require("beautiful").init(require("gears").filesystem.get_configuration_dir() .. "themes/" .. theme .. "/theme.lua")

F = {}
S = {}

require "squeals"
require "conf"

require "ui.notifs"
require "ui.bar"
