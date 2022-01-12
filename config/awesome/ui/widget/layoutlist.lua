local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local wibox = require "wibox"

local layoutlist = awful.popup {
  widget = {
    awful.widget.layoutlist {
      source = awful.widget.layoutlist.source.default_layouts,
      screen = 1,
      base_layout = wibox.widget {
        spacing = 5,
        forced_num_cols = 3,
        layout = wibox.layout.grid.vertical,
      },
      widget_template = {
        {
          {
            id = "icon_role",
            forced_height = 25,
            forced_width = 25,
            widget = wibox.widget.imagebox,
          },
          margins = 10,
          widget = wibox.container.margin,
        },
        id = "background_role",
        forced_width = 35,
        forced_height = 35,
        shape = gears.shape.rounded_rect,
        widget = wibox.container.background,
      },
    },
    widget = wibox.container.margin,
    margins = 20,
  },
  bg = beautiful.bg_dark,
  visible = false,
  shape = function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, 10)
  end,
  placement = function(c)
    (awful.placement.top_right)(c, { margins = { top = 50, right = 10 } })
  end,
  ontop = true,
}

local function toggle()
  layoutlist.visible = not layoutlist.visible
end

return toggle
