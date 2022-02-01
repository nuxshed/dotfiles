require "awful.autofocus"

require("beautiful").init(require("gears").filesystem.get_configuration_dir() .. "themes/cafe/theme.lua")

F = {}

require "conf"

require "ui.titlebar"
require "ui.bar"
require "ui.prompt"
