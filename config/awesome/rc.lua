require "awful.autofocus"

_G.theme = "blackout"

require("beautiful").init(require("gears").filesystem.get_configuration_dir() .. "themes/" .. theme .. "/theme.lua")

F = {}
S = {}

require "squeals"
require "conf"

require "ui.notifs"
require "ui.bar"
require "ui.action_popup"
require "ui.root_menu"
require "ui.splash"
require "ui.prompt"
require "ui.sidebar"
require "ui.slidebar"
require "ui.applets.colr"
require "ui.applets.timer"