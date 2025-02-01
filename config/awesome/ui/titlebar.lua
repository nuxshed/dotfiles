local awful = require "awful"
local wibox = require "wibox"
-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
  -- buttons for the titlebar
  local buttons = {
    awful.button({}, 1, function()
      c:activate { context = "titlebar", action = "mouse_move" }
    end),
    awful.button({ "Shift" }, 1, function()
      c:activate { context = "titlebar", action = "mouse_resize" }
    end),
    awful.button({}, 3, function()
      c:activate { context = "titlebar", action = "mouse_resize" }
    end),
  }

  awful.titlebar(c, {
    size = 70,
  }):setup {
    nil,
    {
      {
        {
          widget = awful.titlebar.widget.titlewidget(c),
        },
        widget = wibox.container.place,
        align = "center",
      },
      buttons = buttons,
      layout = wibox.layout.flex.horizontal,
    },
    {
      {
        awful.titlebar.widget.minimizebutton(c),
        awful.titlebar.widget.maximizedbutton(c),
        awful.titlebar.widget.closebutton(c),
        layout = wibox.layout.fixed.horizontal,
        spacing = 20,
      },
      widget = wibox.container.margin,
      margins = 23,
      right = 28,
    },
    layout = wibox.layout.align.horizontal,
  }
end)

-- imagine using titlebars for tiled windows