local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local wibox = require "wibox"
local helpers = require "helpers"

local icon = wibox.widget {
  widget = wibox.widget.imagebox,
  image = gears.filesystem.get_configuration_dir() .. "icons/wifi.svg",
  stylesheet = " * { stroke: "
    .. beautiful.control_button_active_fg
    .. " } circle { fill: "
    .. beautiful.control_button_active_fg
    .. "; }",
  forced_width = 30,
  valign = "center",
  halign = "center",
}

local wifi = wibox.widget {
  {
    icon,
    widget = wibox.container.margin,
    margins = 10,
  },
  widget = wibox.container.background,
  bg = beautiful.control_button_active_bg,
}

helpers.add_hover_cursor(wifi, "hand1")

local s = false -- off
wifi:buttons {
  awful.button({}, 1, function()
    s = not s
    if s then
      wifi.bg = beautiful.control_button_active_bg
      icon.stylesheet =
        " * { stroke: "
          .. beautiful.control_button_active_fg
          .. " } circle { fill: "
          .. beautiful.control_button_active_fg
          .. "; }",
        awful.spawn "nmcli radio wifi on"
    else
      wifi.bg = beautiful.control_button_normal_bg
      icon.stylesheet =
        " * { stroke: "
          .. beautiful.control_button_normal_fg
          .. " } circle { fill: "
          .. beautiful.control_button_normal_fg
          .. "; }",
        awful.spawn "nmcli radio wifi off"
    end
  end),
}

return wifi
