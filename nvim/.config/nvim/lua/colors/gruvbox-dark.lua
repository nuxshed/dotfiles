-- gruvbox dark

local M = {}

function M.base16()
  local base16 = require "base16"
  base16(base16.themes["gruvbox-dark-hard"], true)
end

M.colors = {
  fg = "#ddc7a1",
  bg = "#161819",
  lightbg = "#2d2d2d",
  linebg = "#2d2d2d",
  fgfaded = "#a89984",
  grey = "#928374",
  light_grey = "#bdae93",
  dark_grey = "#1d2021",
  darker_grey = "#181b1c",
  bright = "#fbf1c7",
  red = "#fb4934",
  green = "#b8bb26",
  blue = "#83a598",
  yellow = "#fabd2f",
  magenta = "#d4869b",
  orange = "#fe8019",
  cyan = "#83a598",
}
return M
