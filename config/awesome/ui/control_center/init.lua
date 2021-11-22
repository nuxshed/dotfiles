local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local wibox = require "wibox"

local control_center = awful.popup {
  widget = {
    {
      {
        font = beautiful.font_name .. " 70",
        format = "%H:%M",
        align = "center",
        widget = wibox.widget.textclock,
      },
      {
        {
          widget = awful.widget.watch("fortune -s", 600),
          align = "center",
        },
        widget = wibox.container.background,
        fg = beautiful.fg_dark,
      },
      {
        {
          {
            {
              {
                widget = require "ui.widget.vol_slider",
              },
              {
                widget = require "ui.widget.bri_slider",
              },
              layout = wibox.layout.flex.vertical,
              spacing = 10,
            },
            margins = 20,
            widget = wibox.container.margin,
          },
          widget = wibox.container.background,
          bg = beautiful.bg_normal,
          shape = function(cr, width, height)
            gears.shape.rounded_rect(cr, width, height, 9)
          end,
          forced_height = 110,
        },
        {
          {
            {
              { widget = require "ui.widget.cpu_circle" },
              margins = 25,
              widget = wibox.container.margin,
            },
            widget = wibox.container.background,
            bg = beautiful.bg_normal,
            shape = function(cr, width, height)
              gears.shape.rounded_rect(cr, width, height, 9)
            end,
          },
          {
            {
              { widget = require "ui.widget.ram_circle" },
              margins = 25,
              widget = wibox.container.margin,
            },
            widget = wibox.container.background,
            bg = beautiful.bg_normal,
            shape = function(cr, width, height)
              gears.shape.rounded_rect(cr, width, height, 9)
            end,
          },
          layout = wibox.layout.flex.horizontal,
          spacing = 20,
        },
        layout = wibox.layout.fixed.vertical,
        forced_height = 300,
        spacing = 20,
      },
      {
        {
          {
            { widget = require "ui.control_center.controls.wifi" },
            { widget = require "ui.control_center.controls.bluetooth" },
            { widget = require "ui.control_center.controls.dnd" },
            { widget = require "ui.control_center.controls.night_light" },
            layout = wibox.layout.flex.horizontal,
            spacing = 25,
          },
          widget = wibox.container.margin,
          forced_height = 90,
          top = 20,
          bottom = 20,
          left = 35,
          right = 35,
        },
        widget = wibox.container.background,
        bg = beautiful.bg_normal,
        shape = function(cr, width, height)
          gears.shape.rounded_rect(cr, width, height, 9)
        end,
      },
      layout = wibox.layout.fixed.vertical,
      spacing = 20,
    },
    widget = wibox.container.margin,
    forced_width = 410,
    margins = 25,
  },
  placement = function(c)
    awful.placement.bottom_right(c, { margins = { bottom = 50, right = 10 } })
  end,
  ontop = true,
  visible = false,
  bg = beautiful.bg_dark,
  shape = function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, 10)
  end,
}

local function toggle()
  control_center.visible = not control_center.visible
end

return toggle
