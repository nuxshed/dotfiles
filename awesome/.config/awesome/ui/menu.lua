local awful = require "awful"
local hotkeys_popup = require "awful.hotkeys_popup"
local beautiful = require "beautiful"

myawesomemenu = {
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

mymainmenu = awful.menu {
  items = {
    { "awesome", myawesomemenu },
    { "terminal", terminal },
    { "browser", "brave" },
    { "emacs", "emacs" },
  },
}

mylauncher = awful.widget.launcher { image = beautiful.awesome_icon, menu = mymainmenu }
