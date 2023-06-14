require "awful.autofocus"

_G.theme = "hydrangea"

require("beautiful").init(require("gears").filesystem.get_configuration_dir() .. "themes/" .. theme .. "/theme.lua")

F = {}

require "squeals"
require "conf"

require "ui.bar"
require "ui.notifs"
require "ui.prompt.exec"
require "ui.prompt.run"
require "ui.titlebar"
require "ui.utilities.exit"
require "ui.utilities.theme_switch"
require "ui.widget.music"
