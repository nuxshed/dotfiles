local awful = require "awful"
local wibox = require "wibox"
local beautiful = require "beautiful"
local helpers = require "helpers"

local start_icon = wibox.widget {
  widget = wibox.container.margin,
  margins = 5,
  buttons = {
    awful.button({}, 1, function()
      F.start.toggle()
    end),
  },
  {
    widget = wibox.widget.imagebox,
    stylesheet = " * { stroke: " .. beautiful.fg_normal .. " }",
    image = beautiful.start_icon,
  },
}

helpers.add_hover_cursor(start_icon, "hand1")

local bat_arcchart = wibox.widget {
  widget = wibox.container.arcchart,
  start_angle = math.pi / 2,
  thickness = 4,
  value = 100,
  min_value = 0,
  max_value = 100,
  colors = { beautiful.fg_normal },
  bg = beautiful.bg_focus,
}

local battery_percent = wibox.widget {
  widget = wibox.widget.textclock,
  font = beautiful.font_name .. " Bold 10",
  valign = "center",
  align = "center",
  text = "lol",
}

local battery_circle = wibox.widget {
  value = 0,
  border_width = 6,
  forced_width = 100,
  forced_height = 100,
  widget = wibox.container.radialprogressbar,
  color = beautiful.fg_normal,
  border_color = beautiful.bg_focus,
  {
    battery_percent,
    widget = wibox.container.margin,
    margins = 20,
  },
}

local battery_popup = awful.popup {
  widget = {
    widget = wibox.container.margin,
    margins = 15,
    battery_circle,
  },
  bg = beautiful.bg_dark,
  visible = false,
  border_width = 2,
  border_color = beautiful.bg_focus,
  placement = function(c)
    (awful.placement.bottom_left)(c, { margins = { left = 65, bottom = 50 } })
  end,
  ontop = true,
}

local battery = wibox.widget {
  bg = beautiful.bg_subtle,
  widget = wibox.container.background,
  buttons = {
    awful.button({}, 1, function()
      battery_popup.visible = not battery_popup.visible
    end),
  },
  {
    bat_arcchart,
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

  bat_arcchart.value = capacity
  bat_arcchart.colors = { fill_color }
  battery_percent.text = tostring(capacity) .. "%"
  battery_circle.value = capacity / 100
  battery_circle.color = fill_color
end)

local time = wibox.widget {
  widget = wibox.container.background,
  bg = beautiful.bg_subtle,
  buttons = {
    awful.button({}, 1, function()
      require "ui.widget.calendar"()
    end),
  },
  {
    widget = wibox.container.margin,
    margins = 7,
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

helpers.add_hover_cursor(time, "hand1")

local time_t = awful.tooltip {
  objects = { time },
  delay_show = 1,
  timer_function = function()
    return os.date "%A %B %d %Y"
  end,
}

local layoutbox = wibox.widget {
  bg = beautiful.bg_subtle,
  fg = beautiful.fg_time,
  widget = wibox.container.background,
  buttons = {
    awful.button({}, 1, function()
      require "ui.widget.layoutlist"()
    end),
    awful.button({}, 4, function()
      awful.layout.inc(1)
    end),
    awful.button({}, 5, function()
      awful.layout.inc(-1)
    end),
  },
  {
    widget = wibox.container.margin,

    margins = 10,
    {
      widget = awful.widget.layoutbox,
    },
  },
}

helpers.add_hover_cursor(layoutbox, "hand1")

screen.connect_signal("request::desktop_decoration", function(s)
  local l = awful.layout.suit
  awful.tag({ "1", "2", "3", "4", "5" }, s, { l.tile, l.tile, l.tile, l.tile, l.tile })

  awful.popup({
    placement = function(c)
      (awful.placement.left + awful.placement.maximize_vertically)(c)
    end,
    screen = s,
    widget = {
      {
        {
          {
            start_icon,
            require "ui.bar.taglist"(s),
            layout = wibox.layout.fixed.vertical,
            spacing = 10,
          },
          widget = wibox.container.margin,
          margins = 5,
        },
        -- {
        --   widget = wibox.container.place,
        --   halign = "center",
        --   {
        --     widget = require "ui.bar.tasklist"(s),
        --   },
        -- },
        nil,
        {
          { widget = battery },
          { widget = time },
          { widget = layoutbox },
          layout = wibox.layout.fixed.vertical,
          spacing = 10,
          bottom = 10,
        },
        layout = wibox.layout.align.vertical,
        forced_width = 35,
      },
      widget = wibox.container.margin,
      margins = 10,
    },
  }):struts { left = 50 }
end)
