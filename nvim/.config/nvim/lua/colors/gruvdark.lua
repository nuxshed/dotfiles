-- gruvdark

local M = {}

M.base16 = require "base16"(require("base16").themes["gruvbox-dark-hard"], true)

M.colors = {
  fg = "#d5c4a1",
  bg = "#111111",
  lightbg = "#262626",
  linebg = "#262626",
  fgfaded = "#a89984",
  grey = "#928374",
  light_grey = "#bdae93",
  dark_grey = "#191919",
  darker_grey = "#161616",
  bright = "#fbf1c7",
  red = "#fb4934",
  green = "#b8bb26",
  blue = "#458588",
  yellow = "#fabd2f",
  magenta = "#d3869b",
  orange = "#fe8019",
  cyan = "#83a598",
}

return M
