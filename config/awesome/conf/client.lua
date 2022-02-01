local awful = require "awful"

client.connect_signal("manage", function(c)
  -- Set the windows at the slave,
  if not awesome.startup then
    awful.client.setslave(c)
  end
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
  c:activate { context = "mouse_enter", raise = false }
end)
