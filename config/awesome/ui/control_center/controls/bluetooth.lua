local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local wibox = require "wibox"

local bluetooth = wibox.widget {
  {
    {
      widget = wibox.widget.imagebox,
      image = gears.filesystem.get_configuration_dir() .. "icons/bluetooth.svg",
      stylesheet = " * { stroke: " .. beautiful.bg_normal .. " }",
      forced_width = 25,
      valign = "center",
      halign = "center",
    },
    widget = wibox.container.margin,
    margins = 12.5,
  },
  widget = wibox.container.background,
  bg = beautiful.control_center_button_bg_off,
  shape = function(cr, width, height)
    gears.shape.squircle(cr, width, height, 2, 0)
  end,
}

-- thanks to nes
local on = beautiful.control_center_button_bg
local off = beautiful.control_center_button_bg_off
local s = true
bluetooth:buttons {
  awful.button({}, 1, function()
    s = not s
    if s then
      bluetooth.bg = off
      awful.spawn "bluetoothctl power off"
    else
      bluetooth.bg = on
      awful.spawn "bluetoothctl power on"
    end
  end),
}

return bluetooth
