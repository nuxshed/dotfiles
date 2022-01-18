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
  font = beautiful.font_name .. " Bold 24",
  widget = wibox.widget.textbox,
}

local weather_feels_like = wibox.widget {
  text = "",
  align = "center",
  widget = wibox.widget.textbox,
}

local weather = wibox.widget {
  weather_description,
  weather_temperature,
  weather_feels_like,
  spacing = 5,
  layout = wibox.layout.fixed.vertical,
}

awesome.connect_signal("squeal::weather", function(temperature, description, feels_like)
  weather_description.markup = description
  weather_temperature.markup = temperature .. "°C"
  weather_feels_like.markup = "Feels like " .. feels_like .. "°C"
end)

return weather
