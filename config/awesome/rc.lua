require "awful.autofocus"
local naughty = require "naughty"

naughty.connect_signal("request::display_error", function(message, startup)
  naughty.notification {
    urgency = "critical",
    title = "Oops, an error happened" .. (startup and " during startup!" or "!"),
    message = message,
  }
end)

require("beautiful").init(require("gears").filesystem.get_configuration_dir() .. "themes/forest/theme.lua")

F = {}

require "squeals"
require "conf"

require "ui.action"
require "ui.bar"
require "ui.exec"
require "ui.exit"
require "ui.notifs"
require "ui.run"
require "ui.scr"
require "ui.start"
require "ui.titlebar"
