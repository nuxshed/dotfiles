local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local wibox = require "wibox"

local wifi = wibox.widget {
  {
    {
      widget = wibox.widget.imagebox,
      image = gears.filesystem.get_configuration_dir() .. "icons/wifi.svg",
      stylesheet = " * { stroke: " .. beautiful.bg_normal .. " }",
      forced_width = 30,
      valign = "center",
      halign = "center",
    },
    widget = wibox.container.margin,
    margins = 10,
  },
  widget = wibox.container.background,
  bg = beautiful.control_center_button_bg,
  shape = function(cr, width, height)
    gears.shape.squircle(cr, width, height, 2, 0)
  end,
}

local on = beautiful.control_center_button_bg
local off = beautiful.control_center_button_bg_off
local s = false -- off
wifi:buttons {
  awful.button({}, 1, function()
    s = not s
    if s then
      wifi.bg = off
      awful.spawn "nmcli radio wifi off"
    else
      wifi.bg = on
      awful.spawn "nmcli radio wifi on"
    end
  end),
}

return wifi
