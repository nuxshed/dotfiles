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
    position = "left",
  }):setup {
    {
      awful.titlebar.widget.closebutton(c),
      awful.titlebar.widget.maximizedbutton(c),
      awful.titlebar.widget.minimizebutton(c),
      layout = wibox.layout.fixed.vertical,
      spacing = 15,
    },
    widget = wibox.container.margin,
    top = 20,
    left = 13,
    right = 13,
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
