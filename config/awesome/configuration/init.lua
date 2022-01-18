local awful = require "awful"
terminal = "kitty"
editor = os.getenv "EDITOR" or "nano"
editor_cmd = terminal .. " -e " .. editor
modkey = "Mod4"

require "configuration.bound"
require "configuration.client"
require "configuration.layout"
require "configuration.ruled"
