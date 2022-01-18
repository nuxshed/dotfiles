local beautiful = require "beautiful"
local gears = require "gears"

client.connect_signal("request::manage", function(c)
  if c.class == "kitty" then
    local i = gears.surface(require("gears").filesystem.get_configuration_dir() .. "/icons/terminal.svg")
    c.icon = i._native
  end
end)
