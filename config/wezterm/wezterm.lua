local wezterm = require("wezterm")
local config = {
	font = wezterm.font("Cartograph CF"),
	color_scheme = "bberry",
	enable_tab_bar = false,
window_padding = {
  left = 30,
  right = 20,
  top = 15,
  bottom = 5,
}
}

config.color_schemes = {
  ["bberry"] = {
    foreground = "#c6c6c6",
    background = "#222228",
    cursor_bg = "#929292",
    cursor_border = "#929292",
    cursor_fg = "#929292",
    selection_bg = "#616c96",
    selection_fg = "#c6c6c6",
    
    ansi = { "#282b33", "#e1c1ee", "#8ca378", "#cfcf9c", "#819cd6", "#b0a2e7", "#515462", "#c6c6c6" },
    brights = { "#404a55", "#e1c1ee", "#8ca378", "#cfcf9c", "#819cd6", "#b0a2e7", "#727269", "#eceff4" },
  },
}

return config
