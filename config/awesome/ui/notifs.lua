local naughty = require "naughty"
local gears = require "gears"
local wibox = require "wibox"
local beautiful = require "beautiful"

-- Default notification template
naughty.connect_signal("request::display", function(n)
  -- Notification icon
  local icon = wibox.widget {
    {
      image = n.icon or beautiful.notification_icon,
      resize = true,
      widget = wibox.widget.imagebox,
    },
    margins = 10,
    widget = wibox.container.margin,
  }

  -- Notification title
  local title = wibox.widget {
    text = n.title or "Notification",
    -- align = "center",
    font = beautiful.font_name .. " 16",
    widget = wibox.widget.textbox,
  }

  -- Notification message
  local message = wibox.widget {
    text = n.message or "This is a message",
    -- align = "center",
    font = beautiful.font_name .. " 16",
    widget = wibox.widget.textbox,
  }

  -- Notification layout
  naughty.layout.box {
    notification = n,
    type = "notification",
    shape = gears.shape.rounded_rect,
    widget_template = {
      {
        {
          -- icon,
          {
            {
	      {
		 title,
		 widget = wibox.container.background,
		 bg = beautiful.bg_focus,
	      },
              message,
              layout = wibox.layout.fixed.vertical,
	      spacing = 5,
            },
            margins = 10,
            widget = wibox.container.margin,
          },
          layout = wibox.layout.align.horizontal,
        },
        margins = 10,
        widget = wibox.container.margin,
      },

      bg = beautiful.bg_normal,
      widget = wibox.container.background,
      border_width = 4,
      border_color = beautiful.bg_focus,
    },
  }
end)
