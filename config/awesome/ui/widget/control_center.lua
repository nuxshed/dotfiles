local awful = require "awful"
local beautiful = require "beautiful"
local naughty = require "naughty"
local wibox = require "wibox"

local actions = wibox.widget {
  {
    {
      {
        widget = require "ui.controls.vol_slider",
      },
      {
        widget = require "ui.controls.bri_slider",
      },
      layout = wibox.layout.flex.vertical,
      spacing = 10,
    },
    {
      { widget = require "ui.controls.wifi" },
      { widget = require "ui.controls.bluetooth" },
      { widget = require "ui.controls.dnd" },
      { widget = require "ui.controls.night_light" },
      layout = wibox.layout.flex.horizontal,
      spacing = 15,
    },
    layout = wibox.layout.flex.vertical,
    spacing = 30,
  },
  widget = wibox.container.margin,
  top = 20,
  bottom = 20,
  left = 25,
  right = 25,
}

local control_center = awful.popup {
  widget = {
    widget = wibox.container.margin,
    margins = 10,
    forced_width = 300,
    forced_height = 200,
    actions,
  },
  border_color = beautiful.fg_minimize,
  border_width = 5,
  bg = beautiful.bg_subtle,
  placement = function(c)
    (awful.placement.bottom_left)(c, { margins = { bottom = 20, left = 70 } })
  end,
  ontop = true,
  visible = false,
}

local function toggle()
  control_center.visible = not control_center.visible
end

return toggle
