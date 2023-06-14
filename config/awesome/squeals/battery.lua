-- Provides:
-- squeal::battery
--      percentage (integer)

local awful = require "awful"

local update_interval = 30

local bat_script = [[sh -c '
  echo "$(cat /sys/class/power_supply/BAT1/capacity)" "$(cat /sys/class/power_supply/BAT1/status)"
']]

awful.widget.watch(bat_script, update_interval, function(_, stdout)
  local capacity = string.sub(stdout, 1, 2)
  local status = string.sub(stdout, 4)
  awesome.emit_signal("squeal::battery", tonumber(capacity), status)
end)
