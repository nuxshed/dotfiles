local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local helpers = require "helpers"
local wibox = require "wibox"

local time = wibox.widget {
  font = "Iosevka Nerd Font Mono Bold 80",
  format = "%H:%M",
  align = "center",
  widget = wibox.widget.textclock,
}

local date = wibox.widget {
  {
    {
      {
        font = beautiful.font_name .. " Medium 9",
        format = "<span foreground='" .. beautiful.fg_dark .. "'>%A</span>",
        widget = wibox.widget.textclock,
      },
      {
        font = beautiful.font_name .. " Bold 14",
        format = "%d %B",
        widget = wibox.widget.textclock,
      },
      widget = wibox.layout.fixed.vertical,
      spacing = 5,
    },
    widget = wibox.container.margin,
    margins = 20,
  },
  widget = wibox.widget.background,
  bg = beautiful.bg_normal,
  shape = helpers.rrect(9),
  forced_height = 85,
}

local mem = wibox.widget {
  {
    { widget = require "ui.widget.ram_circle" },
    margins = 20,
    widget = wibox.container.margin,
  },
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
  shape = helpers.rrect(9),
  forced_height = 150,
}

local cpu = wibox.widget {
  {
    { widget = require "ui.widget.cpu_circle" },
    margins = 20,
    widget = wibox.container.margin,
  },
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
  shape = helpers.rrect(9),
  forced_height = 150,
}

local music = wibox.widget {
  {
    widget = require "ui.sidebar.music",
  },
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
  shape = helpers.rrect(9),
  forced_height = 195,
}

local weather = wibox.widget {
  {
    {
      widget = require "ui.widget.weather",
    },
    widget = wibox.container.margin,
    margins = 20,
  },
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
  shape = helpers.rrect(9),
  forced_height = 120,
}

-- local notifs = wibox.widget {
--   widget = wibox.container.background,
--   bg = beautiful.bg_normal,
--   forced_height = 170,
--   shape = helpers.rrect(9)
--   {
--     widget = wibox.container.margin,
--     margins = 20,
--     {
--       widget = require "ui.sidebar.notifs",
--     },
--   },
-- }

local pfp = wibox.widget {
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
  shape = helpers.rrect(9),
  forced_height = 160,
  {
    widget = wibox.container.margin,
    margins = 20,
    {
      layout = wibox.layout.fixed.vertical,
      spacing = 10,
      {
        widget = wibox.widget.imagebox,
        image = "/home/advait/Downloads/pfp.png",
        clip_shape = gears.shape.circle,
        forced_height = 90,
        halign = "center",
      },
      {
        widget = wibox.widget.textbox,
        font = beautiful.font_name .. " Bold 12",
        markup = "@nuxsh",
        align = "center",
      },
    },
  },
}

local sidebar = awful.popup {
  widget = {
    {
      time,
      {
        {
          pfp,
          cpu,
          weather,
          widget = wibox.layout.fixed.vertical,
          spacing = 25,
        },
        {
          date,
          mem,
          music,
          widget = wibox.layout.fixed.vertical,
          spacing = 25,
        },
        layout = wibox.layout.flex.horizontal,
        spacing = 25,
      },
      layout = wibox.layout.fixed.vertical,
      spacing = 25,
    },
    widget = wibox.container.margin,
    forced_width = 375,
    margins = 25,
  },
  placement = function(c)
    awful.placement.top_left(c, { margins = { top = 60, left = 15 } })
  end,
  ontop = true,
  visible = false,
  bg = beautiful.bg_dark,
  shape = helpers.rrect(9),
}

local function toggle()
  sidebar.visible = not sidebar.visible
end

return toggle
