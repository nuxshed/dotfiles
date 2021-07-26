-- gruvbox dark

local M = {}

function M.base16()
  local base16 = require "base16"
  base16(base16.themes["nvchad-nord"], true)
end

M.colors = {
  fg = "#d8dee9",
  bg = "#121419",
  lightbg = "#2e3440",
  linebg = "#2e3440",
  fgfaded = "#565c68",
  grey = "#3e4656",
  light_grey = "#4c566a",
  dark_grey = "#1f232b",
  darker_grey = "#16181e",
  bright = "#eceff4",
  red = "#bf616a",
  green = "#a3be8c",
  blue = "#81a1c1",
  yellow = "#ebcb8b",
  magenta = "#b48ead",
  orange = "#d08770",
  cyan = "#9fe2f4",
}
return M
