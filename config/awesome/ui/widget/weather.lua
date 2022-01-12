local wibox = require "wibox"
local beautiful = require "beautiful"

local weather_description = wibox.widget {
  text = "Loading...",
  align = "center",
  widget = wibox.widget.textbox,
}

local weather_temperature = wibox.widget {
  text = "",
  align = "center",
  font = beautiful.font_name .. " Bold 20",
  widget = wibox.widget.textbox,
}

local weather = wibox.widget {
  weather_description,
  weather_temperature,
  spacing = 4,
  layout = wibox.layout.fixed.vertical,
}

awesome.connect_signal("squeal::weather", function(temperature, description, icon_code)
  weather_description.markup = description
  weather_temperature.markup = temperature .. "°C"
end)

return weather
