local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local naughty = require "naughty"
local wibox = require "wibox"
local helpers = require "helpers"

local icon = wibox.widget {
  widget = wibox.widget.imagebox,
  image = gears.filesystem.get_configuration_dir() .. "icons/moon.svg",
  stylesheet = " * { stroke: " .. beautiful.fg_normal .. " }",
  forced_width = 28,
  valign = "center",
  halign = "center",
}

local dnd = wibox.widget {
  {
    icon,
    widget = wibox.container.margin,
    margins = 11,
  },
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
}

helpers.add_hover_cursor(dnd, "hand1")

local s = true
dnd:buttons {
  awful.button({}, 1, function()
    s = not s
    if s then
      dnd.bg = beautiful.control_button_normal_bg
      icon.stylesheet = " * { stroke: " .. beautiful.control_button_normal_fg .. " }", naughty.resume()
    else
      dnd.bg = beautiful.control_button_active_bg
      icon.stylesheet = " * { stroke: " .. beautiful.control_button_active_fg .. " }", naughty.suspend()
    end
  end),
}

return dnd
