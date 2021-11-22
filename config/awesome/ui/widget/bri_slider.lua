local awful = require "awful"
local gears = require "gears"
local wibox = require "wibox"
local beautiful = require "beautiful"

local slider = wibox.widget {
  bar_shape = function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, 2.5)
  end,
  bar_height = 10,
  bar_color = beautiful.bg_focus,
  bar_active_color = beautiful.control_center_bri_slider_active,
  handle_width = 0,
  value = 25,
  widget = wibox.widget.slider,
}

local bri_slider = wibox.widget {
  {
    widget = wibox.widget.imagebox,
    image = gears.filesystem.get_configuration_dir() .. "icons/sun.svg",
    stylesheet = " * { stroke: " .. beautiful.fg_normal .. " }",
    forced_width = 19,
    valign = "center",
    halign = "center",
  },
  slider,
  layout = wibox.layout.fixed.horizontal,
  spacing = 15,
}

slider:connect_signal("property::value", function(_, value)
  awful.spawn.with_shell("bri " .. value .. "%")
end)

return bri_slider
