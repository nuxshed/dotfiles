local awful = require "awful"
local beautiful = require "beautiful"
local naughty = require "naughty"
local wibox = require "wibox"

naughty.config.defaults.ontop = true
naughty.config.defaults.screen = awful.screen.focused()
naughty.config.defaults.timeout = 10
naughty.config.defaults.title = "Notification"
naughty.config.defaults.position = "top_right"
naughty.config.defaults.border_width = 2
naughty.config.defaults.border_color = beautiful.bg_subtle

naughty.config.presets.normal = {
  font = beautiful.font,
  fg = beautiful.fg_normal,
  bg = beautiful.bg_normal,
}

naughty.config.presets.low = {
  font = beautiful.font,
  fg = beautiful.fg_normal,
  bg = beautiful.bg_normal,
}

naughty.config.presets.ok = naughty.config.presets.normal
naughty.config.presets.info = naughty.config.presets.normal
naughty.config.presets.warn = naughty.config.presets.critical

naughty.connect_signal("request::display", function(n)
  if not n.app_icon then
    n.app_icon = beautiful.notification_icon
  end

  n.title = "<span font='" .. beautiful.font_name .. "10'><b>" .. n.title .. "</b></span>"

  local time = os.date "%H:%M"

  naughty.layout.box {
    notification = n,
    type = "notification",
    bg = beautiful.bg_normal,
    widget_template = {
      {
        {
          {
            {
              naughty.widget.icon,
              {
                naughty.widget.title,
                naughty.widget.message,
                spacing = 4,
                layout = wibox.layout.fixed.vertical,
              },
              fill_space = true,
              spacing = 10,
              layout = wibox.layout.fixed.horizontal,
            },
            naughty.list.actions,
            spacing = 10,
            layout = wibox.layout.fixed.vertical,
          },
          margins = 20,
          bottom = 10,
          widget = wibox.container.margin,
        },
        id = "background_role",
        widget = naughty.container.background,
      },
      strategy = "max",
      width = 600,
      widget = wibox.container.constraint,
    },
  }
end)
