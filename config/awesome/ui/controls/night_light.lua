local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local wibox = require "wibox"
local helpers = require "helpers"

local night_light = wibox.widget {
  {
    {
      widget = wibox.widget.imagebox,
      image = gears.filesystem.get_configuration_dir() .. "icons/sun.svg",
      stylesheet = " * { stroke: " .. beautiful.fg_normal .. " }",
      forced_width = 25,
      valign = "center",
      halign = "center",
    },
    widget = wibox.container.margin,
    margins = 12.5,
  },
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
}

helpers.add_hover_cursor(night_light, "hand1")

local on = beautiful.bg_focus
local off = beautiful.bg_normal
local s = true
night_light:buttons {
  awful.button({}, 1, function()
    s = not s
    if s then
      night_light.bg = off
      awful.spawn "pkill -USR1 redshift"
    else
      night_light.bg = on
      awful.spawn "pkill -USR1 redshift"
    end
  end),
}

return night_light
