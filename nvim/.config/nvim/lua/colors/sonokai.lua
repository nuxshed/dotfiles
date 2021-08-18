-- onedark(er)

local M = {}

function M.base16()
  local base16_sonokai = require("base16").theme_from_array {
    "181819",
    "2c2e34",
    "3b3e48",
    "7f8490",
    "70747f",
    "e2e2e3",
    "f2f2f3",
    "2c2e34",
    "e2e2e3",
    "b885f2",
    "76cce0",
    "e7c664",
    "9ed072",
    "9ed072",
    "fc5d7c",
    "fc5d7c",
  }
  local base16 = require "base16"
  base16(base16_sonokai, true)
end

M.colors = {
  fg = "#e2e2e3",
  bg = "#181819",
  lightbg = "#2c2e34",
  linebg = "#2c2e34",
  fgfaded = "#7f8490",
  grey = "#60605e",
  light_grey = "#70747f",
  dark_grey = "#222428",
  darker_grey = "#1c1d21",
  bright = "#f2f2f3",
  red = "#fc5d7c",
  green = "#9ed072",
  blue = "#76cce0",
  yellow = "#e7c664",
  magenta = "#b885f2",
  orange = "#f39660",
  cyan = "#56b6c2",
  ViMode = {},
}

M.overrides = function() end

return M
