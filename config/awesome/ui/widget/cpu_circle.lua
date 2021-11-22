local gears = require "gears"
local wibox = require "wibox"
local beautiful = require "beautiful"

local cpu_circle = wibox.widget {
  {
    {
      widget = wibox.widget.imagebox,
      image = gears.filesystem.get_configuration_dir() .. "icons/cpu.svg",
      stylesheet = " * { stroke: " .. beautiful.fg_normal .. " }",
      valign = "center",
      halign = "center",
    },
    widget = wibox.container.margin,
    margins = 40,
  },
  value = 0.5,
  max_value = 1,
  min_value = 0,
  color = beautiful.control_center_cpu_active,
  border_color = beautiful.bg_focus,
  border_width = 7,
  widget = wibox.container.radialprogressbar,
}

awesome.connect_signal("squeal::cpu", function(used)
  cpu_circle.value = used / 100
end)

return cpu_circle
