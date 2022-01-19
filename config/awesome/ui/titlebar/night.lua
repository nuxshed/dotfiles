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
    size = 37.5,
  }):setup {
    {
      forced_width = 50,
      layout = wibox.layout.fixed.vertical,
    },
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
        awful.titlebar.widget.closebutton(c),
        layout = wibox.layout.fixed.horizontal,
        spacing = 5,
      },
      widget = wibox.container.margin,
      margins = 12,
    },
    layout = wibox.layout.align.horizontal,
  }
end)

-- imagine (not) using titlebars for tiled windows
-- screen.connect_signal("arrange", function(s)
--   local layout = s.selected_tag.layout.name
--   for _, c in pairs(s.clients) do
--     if layout == "floating" or c.floating then
--       awful.titlebar.show(c)
--     else
--       awful.titlebar.hide(c)
--     end
--   end
-- end)
