M = {}

local awful = require "awful"

M.main = awful.menu {
  items = {
    { "Terminal", C.terminal },
    { "Browser", C.browser },
    { "Editor", C.editor },
    {
      "Configure",
      {
        {
          "Change theme",
          function()
            F.theme_switch.toggle()
          end,
        },
        {
          "Edit config",
          C.editor .. " " .. require("gears").filesystem.get_configuration_dir() .. "/rc.lua",
        },
      },
    },
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
