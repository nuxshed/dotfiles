pcall(require, "luarocks.loader")

require "awful.autofocus"
local beautiful = require "beautiful"
local naughty = require "naughty"

-- err handling
naughty.connect_signal("request::display_error", function(message, startup)
  naughty.notification {
    urgency = "critical",
    title = "Oops, an error happened" .. (startup and " during startup!" or "!"),
    message = message,
  }
end)

-- truly beautiful
beautiful.init(require("gears").filesystem.get_configuration_dir() .. "themes/night/theme.lua")

-- *gulp*
require("modules.bling").module.window_swallowing.start()

-- secrets
require "secrets"

-- config stuff
require "configuration"

-- user interfaces
require("ui.bar." .. beautiful.bar_type)
require("ui.titlebar." .. beautiful.titlebar)
require "ui.menu"
require "ui.notifs"
require "ui.sidebar"
require "ui.action_center"

-- signals
require "squeals"
