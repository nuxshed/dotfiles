local awful = require "awful"
local wibox = require "wibox"
local beautiful = require "beautiful"
local helpers = require "helpers"

local bat_bar = wibox.widget {
  widget = wibox.widget.progressbar,
  forced_width = 20,
  forced_height = 10,
  value = 100,
  min_value = 0,
  max_value = 100,
  colors = { beautiful.fg_normal },
  bg = beautiful.bg_focus,
}

local battery = wibox.widget {
  bg = beautiful.bg_normal,
  fg = beautiful.bar_battery_fg or beautiful.bar_status_fg,
  widget = wibox.container.background,
  buttons = {
    awful.button({}, 1, function()
      require "ui.widget.control_center"()
    end),
  },
  {
    bat_bar,
    widget = wibox.container.margin,
    margins = 7,
    bottom = 8,
  },
}

helpers.add_hover_cursor(battery, "hand1")

awesome.connect_signal("squeal::battery", function(capacity, status)
  local fill_color = beautiful.fg_normal

  if capacity >= 11 and capacity <= 35 then
    fill_color = beautiful.warn
  elseif capacity <= 10 then
    fill_color = beautiful.critical
  end

  if status == "Charging\n" then
    fill_color = beautiful.green
  end

  bat_bar.value = capacity
  bat_bar.color = fill_color
  bat_bar.background_color = beautiful.bg_focus
end)

local time = wibox.widget {
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
  {
    widget = wibox.container.margin,
    margins = 5,
    {
      layout = wibox.layout.fixed.vertical,
      {
        widget = wibox.widget.textclock "%H",
        font = beautiful.font_name .. " Bold 10",
        align = "center",
      },
      {
        widget = wibox.widget.textclock "%M",
        font = beautiful.font_name .. " Bold 10",
        align = "center",
      },
    },
  },
}

local time_t = awful.tooltip {
  objects = { time },
  timer_function = function()
    return os.date "%A %B %d %Y"
  end,
}

screen.connect_signal("request::desktop_decoration", function(s)
  local l = awful.layout.suit
  awful.tag({ "1", "2", "3", "4", "5" }, s, { l.tile, l.tile, l.tile, l.tile, l.tile })

  awful
    .popup({
      placement = function(c)
        (awful.placement.left + awful.placement.maximize_vertically)(c)
      end,
      screen = s,
      bg = "#00000000",
      widget = {
        {
          {
            {
              {
                require "ui.bar.taglist"(s),
                layout = wibox.layout.fixed.vertical,
                spacing = 10,
              },
              widget = wibox.container.margin,
              margins = 5,
            },
            widget = wibox.container.background,
            bg = beautiful.bg_normal,
          },
          nil,
          {
            { widget = battery },
            { widget = time },
            layout = wibox.layout.fixed.vertical,
            spacing = 10,
            right = 10,
          },
          layout = wibox.layout.align.vertical,
          forced_width = 35,
        },
        widget = wibox.container.margin,
        margins = 20,
      },
    })
    :struts { left = 60 }
end)
