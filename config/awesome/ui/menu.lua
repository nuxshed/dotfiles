local awful = require "awful"
local hotkeys_popup = require "awful.hotkeys_popup"

_G.Menu = {}

Menu.awesome = {
  {
    "hotkeys",
    function()
      hotkeys_popup.show_help(nil, awful.screen.focused())
    end,
  },
  { "edit config", editor_cmd .. " " .. awesome.conffile },
  { "restart", awesome.restart },
  {
    "quit",
    function()
      awesome.quit()
    end,
  },
}

Menu.main = awful.menu {
  items = {
    { "awesome", Menu.awesome },
    { "terminal", terminal },
    { "browser", "brave" },
    { "emacs", "emacs" },
  },
}
