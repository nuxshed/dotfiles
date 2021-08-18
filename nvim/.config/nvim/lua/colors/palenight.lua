-- palenight

local M = {}

function M.base16()
  local base16 = require "base16"
  local base16_palenight = base16.theme_from_array {
    "1f2230",
    "32374c",
    "32374D",
    "697098",
    "8796B0",
    "bfc7d5",
    "fbf1c7",
    "32374c",
    "bfc7d5",
    "F78C6C",
    "ffcb6b",
    "C3E88D",
    "80CBC4",
    "82b1ff",
    "c792ea",
    "ff5370",
  }
  base16(base16_palenight, true)
  vim.cmd "hi TsTag guifg=#ff5370"
  vim.cmd "hi TsTagDelimiter guifg=#89DDFF"
end

M.colors = {
  fg = "#bfc7d5",
  bg = "#1f2230",
  lightbg = "#32374c",
  linebg = "#32374c",
  fgfaded = "#697098",
  grey = "#3E4452",
  light_grey = "#4B5263",
  dark_grey = "#282c3d",
  darker_grey = "#1c1f2c",
  bright = "#fbf1c7",
  red = "#ff5370",
  green = "#C3E88D",
  blue = "#82b1ff",
  yellow = "#ffcb6b",
  magenta = "#c792ea",
  orange = "#F78C6C",
  cyan = "#89DDFF",
  ViMode = {},
}

M.overrides = function() end

return M
