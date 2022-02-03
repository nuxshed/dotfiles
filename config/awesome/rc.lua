require "awful.autofocus"
local naughty = require "naughty"

naughty.connect_signal("request::display_error", function(message, startup)
  naughty.notification {
    urgency = "critical",
    title = "Oops, an error happened" .. (startup and " during startup!" or "!"),
    message = message,
  }
end)

require("beautiful").init(require("gears").filesystem.get_configuration_dir() .. "themes/cafe/theme.lua")

F = {}

require "conf"

require "ui.titlebar"
require "ui.bar"
require "ui.exec"
require "ui.run"
require "ui.exit"
require "ui.scr"
