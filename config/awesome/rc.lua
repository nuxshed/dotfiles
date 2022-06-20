require "awful.autofocus"
local naughty = require "naughty"

naughty.connect_signal("request::display_error", function(message, startup)
  naughty.notification {
    urgency = "critical",
    title = "Oops, an error happened" .. (startup and " during startup!" or "!"),
    message = message,
  }
end)

_G.theme = "material"

require("beautiful").init(require("gears").filesystem.get_configuration_dir() .. "themes/" .. theme .. "/theme.lua")

F = {}

require "squeals"
require "conf"

require "ui.action"
require "ui.bar"
require "ui.notifs"
require "ui.prompt.exec"
require "ui.prompt.run"
require "ui.start"
require "ui.titlebar"
require "ui.utilities.exit"
require "ui.utilities.scr"
require "ui.utilities.theme_switch"
