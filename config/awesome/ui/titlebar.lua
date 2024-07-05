local awful = require "awful"
local wibox = require "wibox"
-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
  awful
    .titlebar(c, {
      size = 40,
      position = "left",
    })
    :setup {}
end)
