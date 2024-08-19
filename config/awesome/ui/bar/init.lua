local awful = require "awful"
local wibox = require "wibox"
local beautiful = require "beautiful"
local helpers = require "helpers"

screen.connect_signal("request::desktop_decoration", function(s)
  local l = awful.layout.suit
  awful.tag({ "1", "2", "3", "4", "5" }, s, { l.tile, l.tile, l.tile, l.tile, l.tile })

  awful
    .popup({
      placement = function(c)
        (awful.placement.left + awful.placement.maximize_vertically)(c)
      end,
      screen = s,
      bg = "#00000000",
    })
    :struts { left = 0 }
end)
