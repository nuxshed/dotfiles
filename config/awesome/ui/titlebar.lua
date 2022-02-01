local awful = require "awful"
local wibox = require "wibox"

client.connect_signal("request::titlebars", function(c)
  awful.titlebar(c, {
    size = 37.5,
  }):setup {
    layout = wibox.layout.align.horizontal,
  }
end)

-- imagine using titlebars for tiled windows
screen.connect_signal("arrange", function(s)
  local layout = s.selected_tag.layout.name
  for _, c in pairs(s.clients) do
    if layout == "floating" or c.floating then
      awful.titlebar.show(c)
    else
      awful.titlebar.hide(c)
    end
  end
end)
