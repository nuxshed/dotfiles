require "awful.autofocus"

_G.theme = "bberry"

require("beautiful").init(require("gears").filesystem.get_configuration_dir() .. "themes/" .. theme .. "/theme.lua")

F = {}
S = {}

require "squeals"
require "conf"

require "ui.notifs"
require "ui.bar"
require "ui.action_center"
require "ui.notif_center"
require "ui.prompt"
require "ui.action_popup"
require "ui.root_menu"
