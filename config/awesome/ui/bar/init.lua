local awful = require "awful"
local wibox = require "wibox"
local beautiful = require "beautiful"
local helpers = require "helpers"

local battery = wibox.widget {
  bg = beautiful.bg_normal,
  fg = beautiful.fg_bat,
  shape = helpers.rrect(9),
  widget = wibox.container.background,
  {
    {
      widget = awful.widget.watch("status bat", 30),
    },
    left = 7,
    right = 7,
    top = 5,
    bottom = 5,
    widget = wibox.container.margin,
  },
}

local time = wibox.widget {
  bg = beautiful.bg_normal,
  fg = beautiful.fg_time,
  shape = helpers.rrect(9),
  widget = wibox.container.background,
  {
    widget = wibox.widget.textclock,
  },
}

local layoutbox = wibox.widget {
  bg = beautiful.bg_normal,
  fg = beautiful.fg_time,
  shape = helpers.rrect(9),
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

    margins = 7.5,
    {
      widget = awful.widget.layoutbox,
    },
  },
}

helpers.add_hover_cursor(layoutbox, "hand1")

screen.connect_signal("request::desktop_decoration", function(s)
  local l = awful.layout.suit
  awful.tag(
    { "1", "2", "3", "4", "5", "6", "7", "8", "9" },
    s,
    { l.tile, l.tile, l.tile, l.tile, l.tile, l.tile, l.tile, l.tile, l.tile }
  )

  awful.popup({
    placement = function(c)
      (awful.placement.top + awful.placement.maximize_horizontally)(c)
    end,
    screen = s,
    widget = {
      {
        {
          {
            widget = require "ui.bar.taglist"(s),
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
          layout = wibox.layout.fixed.horizontal,
          spacing = 10,
          bottom = 10,
        },
        layout = wibox.layout.align.horizontal,
        forced_height = 30,
      },
      widget = wibox.container.margin,
      margins = 7,
    },
  }):struts { top = 40 }
end)
