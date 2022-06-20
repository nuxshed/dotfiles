local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local wibox = require "wibox"
local helpers = require "helpers"

local icon = wibox.widget {
  widget = wibox.widget.imagebox,
  image = gears.filesystem.get_configuration_dir() .. "icons/sun.svg",
  stylesheet = " * { stroke: " .. beautiful.control_button_normal_fg .. " }",
  forced_width = 30,
  valign = "center",
  halign = "center",
}

local night_light = wibox.widget {
  {
    icon,
    widget = wibox.container.margin,
    margins = 12.5,
  },
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
}

helpers.add_hover_cursor(night_light, "hand1")

local s = true
night_light:buttons {
  awful.button({}, 1, function()
    s = not s
    if s then
      night_light.bg = beautiful.control_button_normal_bg
      icon.stylesheet =
        " * { stroke: " .. beautiful.control_button_normal_fg .. " }", awful.spawn "pkill -USR1 redshift"
    else
      night_light.bg = beautiful.control_button_active_bg
      icon.stylesheet =
        " * { stroke: " .. beautiful.control_button_active_fg .. " }", awful.spawn "pkill -USR1 redshift"
    end
  end),
}

return night_light
