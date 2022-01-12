local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local wibox = require "wibox"

local time = wibox.widget {
  font = beautiful.font_name .. " Bold 60",
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
    margins = 15,
  },
  widget = wibox.widget.background,
  bg = beautiful.bg_normal,
  shape = function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, 9)
  end,
  forced_height = 75,
}

local mem = wibox.widget {
  {
    { widget = require "ui.widget.ram_circle" },
    margins = 20,
    widget = wibox.container.margin,
  },
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
  shape = function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, 9)
  end,
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
  shape = function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, 9)
  end,
  forced_height = 150,
}

local music = wibox.widget {
  {
    widget = require "ui.widget.music",
  },
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
  shape = function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, 9)
  end,
  forced_height = 175,
}

local weather = wibox.widget {
  {
    {
      widget = require "ui.widget.weather",
    },
    widget = wibox.container.margin,
    left = 25,
    margins = 20,
  },
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
  shape = function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, 9)
  end,
  forced_height = 80,
}

local notifs = wibox.widget {
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
  forced_height = 170,
  shape = function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, 9)
  end,
  {
    widget = wibox.container.margin,
    margins = 20,
    {
      widget = require "ui.sidebar.notifs",
    },
  },
}

local sidebar = awful.popup {
  widget = {
    {
      time,
      {
        {
          mem,
          music,
          widget = wibox.layout.fixed.vertical,
          spacing = 20,
        },
        {
          date,
          cpu,
          weather,
          widget = wibox.layout.fixed.vertical,
          spacing = 20,
        },
        layout = wibox.layout.flex.horizontal,
        spacing = 20,
      },
      notifs,
      layout = wibox.layout.fixed.vertical,
      spacing = 20,
    },
    widget = wibox.container.margin,
    forced_width = 375,
    margins = 25,
  },
  placement = function(c)
    awful.placement.left(c, { margins = { top = 50, left = 10 } })
  end,
  ontop = true,
  visible = false,
  bg = beautiful.bg_dark,
  shape = function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, 10)
  end,
}

local function toggle()
  sidebar.visible = not sidebar.visible
end

return toggle
