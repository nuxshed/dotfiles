local bling = require "modules.bling"
local beautiful = require "beautiful"

F.launchy = {}

local app_launcher = bling.widget.app_launcher {
  apps_per_column = 3,
  apps_per_row = 3,
  terminal = "kitty",
  favourites = { "emacs", "brave", "kitty" },
  search_commands = true,
  app_width = 100,
  app_height = 100,
  app_icon_width = 50,
  app_icon_height = 50,
  app_normal_color = beautiful.bg_normal,
  app_normal_hover_color = beautiful.bg_subtle,
  app_selected_color = beautiful.bg_focus,
  app_selected_hover_color = beautiful.bg_focus,
  prompt_color = beautiful.bg_normal,
  prompt_text_color = beautiful.fg_normal,
  border_color = beautiful.bg_subtle,
  background = beautiful.bg_normal,
}

function F.launchy.toggle()
  app_launcher:toggle()
end
