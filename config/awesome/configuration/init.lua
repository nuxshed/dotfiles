local awful = require "awful"
terminal = "kitty"
editor = os.getenv "EDITOR" or "nano"
editor_cmd = terminal .. " -e " .. editor
modkey = "Mod4"

-- autostart
autostart_apps = {
  "picom",
}

for app = 1, #autostart_apps do
  awful.spawn.single_instance(autostart_apps[app], awful.rules.rules)
end

require "configuration.ruled"
require "configuration.bound"
require "configuration.layout"
