M = {}

local awful = require "awful"

M.main = awful.menu {
  items = {
    { "Terminal", C.terminal },
    { "Browser", C.browser },
    { "Editor", C.editor },
    {
      "Exit",
      {
        { "Log out", "awesome-client 'awesome.quit()'" },
        { "Power off", "poweroff" },
        { "Restart", "reboot" },
      },
    },
  },
}
