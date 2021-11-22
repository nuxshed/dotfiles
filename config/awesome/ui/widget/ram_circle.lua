local gears = require "gears"
local wibox = require "wibox"
local beautiful = require "beautiful"

local ram_circle = wibox.widget {
  {
    {
      widget = wibox.widget.imagebox,
      image = gears.filesystem.get_configuration_dir() .. "icons/memory.svg",
      stylesheet = " * { fill: " .. beautiful.fg_normal .. " }",
      valign = "center",
      halign = "center",
    },
    widget = wibox.container.margin,
    margins = 40,
  },
  value = 0.5,
  max_value = 1,
  min_value = 0,
  color = beautiful.control_center_mem_used,
  border_color = beautiful.bg_focus,
  border_width = 7,
  widget = wibox.container.radialprogressbar,
}

awesome.connect_signal("squeal::ram", function(used, total)
  ram_circle.value = (used / total)
end)

return ram_circle
