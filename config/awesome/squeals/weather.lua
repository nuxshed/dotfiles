-- Provides:
-- squeal::weather
--      temperature (integer)
--      description (string)
local awful = require "awful"
local helpers = require "helpers"
local secrets = require "secrets"

-- Configuration
local key = secrets.weather_key
local city_id = secrets.weather_city
-- Don't update too often, because your requests might get blocked for 24 hours
local update_interval = 1200
local temp_file = "/tmp/awesomewm-squeal-weather-" .. city_id

local stdout_script = [[
    bash -c '
    KEY="]] .. key .. [["
    CITY="]] .. city_id .. [["
    UNITS="metric"
    weather=$(curl -sf "http://api.openweathermap.org/data/2.5/weather?APPID=$KEY&id=$CITY&units=$UNITS")
    if [ ! -z "$weather" ]; then
        weather_temp=$(echo "$weather" | jq ".main.temp" | cut -d "." -f 1)
        weather_description=$(echo "$weather" | jq -r ".weather[].description" | head -1)
        echo "$weather_description"@@"$weather_temp"
    else
        echo "..."
    fi
  ']]

helpers.remote_watch(stdout_script, update_interval, temp_file, function(stdout)
  stdout = string.gsub(stdout, "^%s*(.-)%s*$", "%1")
  -- Replace "-0" with "0" degrees
  stdout = string.gsub(stdout, "%-0", "0")
  -- Capitalize first letter of the description
  stdout = stdout:sub(1, 1):upper() .. stdout:sub(2)
  local description = stdout:match "(.*)@@"
  local temperature = stdout:match "@@(.*)"
  awesome.emit_signal("squeal::weather", tonumber(temperature), description)
end)
