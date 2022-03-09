local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local wibox = require "wibox"
local helpers = require "helpers"

local bluetooth = wibox.widget {
  {
    {
      widget = wibox.widget.imagebox,
      image = gears.filesystem.get_configuration_dir() .. "icons/bluetooth.svg",
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

helpers.add_hover_cursor(bluetooth, "hand1")

-- thanks to nes
local on = beautiful.bg_focus
local off = beautiful.bg_normal
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
