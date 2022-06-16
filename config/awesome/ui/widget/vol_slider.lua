local awful = require "awful"
local gears = require "gears"
local wibox = require "wibox"
local beautiful = require "beautiful"
local helpers = require "helpers"

local slider = wibox.widget {
  bar_shape = require("helpers").rrect(9),
  bar_height = 6,
  bar_color = beautiful.bg_focus,
  bar_active_color = beautiful.slider_active_color or beautiful.fg_normal,
  handle_shape = gears.shape.circle,
  handle_color = beautiful.slider_handle_color or beautiful.fg_minimize,
  handle_width = 12,
  value = 75,
  widget = wibox.widget.slider,
}

helpers.add_hover_cursor(slider, "hand1")

local vol_slider = wibox.widget {
  {
    widget = wibox.widget.imagebox,
    image = gears.filesystem.get_configuration_dir() .. "icons/volume.svg",
    stylesheet = " * { stroke: " .. beautiful.fg_normal .. " }",
    forced_width = 20,
    valign = "center",
    halign = "center",
  },
  slider,
  layout = wibox.layout.fixed.horizontal,
  spacing = 15,
}

slider:connect_signal("property::value", function(_, value)
  awful.spawn.with_shell("amixer sset Master " .. value .. "%")
end)

return vol_slider
