local wezterm = require("wezterm")
local config = {
  font = wezterm.font("Cartograph CF"),
  font_size = 10,
  dpi = 190,
  color_scheme = "ncfiesta",
  enable_tab_bar = false,
  window_padding = {
    left = 60,
    right = 60,
    top = 40,
    bottom = 40,
  },
  adjust_window_size_when_changing_font_size = false,
  -- enable_wayland = false,
}

config.color_schemes = {
  ["ncfiesta"] = {
    foreground = "#E1E1E1",
    background = "#151515",
    cursor_bg = "#E1E1E1",
    cursor_border = "#E1E1E1",
    cursor_fg = "#151515",
    selection_bg = "#373737",
    selection_fg = "#E1E1E1",
    ansi = {
      "#151515",
      "#b46958",
      "#90A959",
      "#F4BF75",
      "#BAD7FF",
      "#AA759F",
      "#88afa2",
      "#E1E1E1",
    },
    brights = {
      "#373737",
      "#b46958",
      "#90A959",
      "#F4BF75",
      "#BAD7FF",
      "#AA759F",
      "#88afa2",
      "#E1E1E1",
    }
  },
}

return config
