local awful = require "awful"
local wibox = require "wibox"
local gears = require "gears"
local beautiful = require "beautiful"

F.splash = {}

local splash_screen = wibox {
  ontop = true,
  visible = false,
  bg = beautiful.bg_normal,
  type = "splash",
  screen = screen.primary,
}

splash_screen.width = screen.primary.geometry.width
splash_screen.height = screen.primary.geometry.height

local time = wibox.widget {
  widget = wibox.widget.textclock,
  align = "center",
  valign = "center",
  font = beautiful.font_name .. " 144",
  format = "%H:%M:%S",
  refresh = 1,
}

local date = wibox.widget {
  widget = wibox.widget.textclock,
  align = "center",
  valign = "center",
  font = beautiful.font_name .. " 24",
  format = "%A, %b %d",
}

splash_screen:setup {
  {
    time,
    date,
    layout = wibox.layout.fixed.vertical,
  },
  widget = wibox.container.place,
}

function F.splash.toggle()
  splash_screen.visible = not splash_screen.visible
end
