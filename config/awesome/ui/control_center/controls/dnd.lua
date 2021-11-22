local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local naughty = require "naughty"
local wibox = require "wibox"

local dnd = wibox.widget {
  {
    {
      widget = wibox.widget.imagebox,
      image = gears.filesystem.get_configuration_dir() .. "icons/moon.svg",
      stylesheet = " * { stroke: " .. beautiful.bg_normal .. " }",
      forced_width = 28,
      valign = "center",
      halign = "center",
    },
    widget = wibox.container.margin,
    margins = 11,
  },
  widget = wibox.container.background,
  bg = beautiful.control_center_button_bg_off,
  shape = function(cr, width, height)
    gears.shape.squircle(cr, width, height, 2, 0)
  end,
}

local on = beautiful.control_center_button_bg
local off = beautiful.control_center_button_bg_off
local s = true
dnd:buttons {
  awful.button({}, 1, function()
    s = not s
    if s then
      dnd.bg = off
      naughty.resume()
    else
      dnd.bg = on
      naughty.suspend()
    end
  end),
}

return dnd
