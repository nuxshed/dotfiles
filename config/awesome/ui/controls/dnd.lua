local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local naughty = require "naughty"
local wibox = require "wibox"
local helpers = require "helpers"

local dnd = wibox.widget {
  {
    {
      widget = wibox.widget.imagebox,
      image = gears.filesystem.get_configuration_dir() .. "icons/moon.svg",
      stylesheet = " * { stroke: " .. beautiful.fg_normal .. " }",
      forced_width = 28,
      valign = "center",
      halign = "center",
    },
    widget = wibox.container.margin,
    margins = 11,
  },
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
}

helpers.add_hover_cursor(dnd, "hand1")

local on = beautiful.bg_focus
local off = beautiful.bg_normal
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
