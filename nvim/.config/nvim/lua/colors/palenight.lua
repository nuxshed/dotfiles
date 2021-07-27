-- palenight

local M = {}

function M.base16()
  local base16 = require "base16"
  base16(base16.themes["material-palenight"], true)
end

M.colors = {
  fg = "#bfc7d5",
  bg = "#1f2230",
  lightbg = "#32374c",
  linebg = "#32374c",
  fgfaded = "#697098",
  grey = "#3E4452",
  light_grey = "#4B5263",
  dark_grey = "#1c1f2d",
  darker_grey = "#1c1f2c",
  bright = "#fbf1c7",
  red = "#ff5370",
  green = "#C3E88D",
  blue = "#82b1ff",
  yellow = "#ffcb6b",
  magenta = "#c792ea",
  orange = "#F78C6C",
  cyan = "#89DDFF",
}
return M
