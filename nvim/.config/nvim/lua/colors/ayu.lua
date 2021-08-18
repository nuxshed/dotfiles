-- ayu
-- WIP

local M = {}

function M.base16()
  local base16_ayu = require("base16").theme_from_array {
    "0A0E14",
    "192333",
    "253340",
    "626a73",
    "b3b1ad",
    "999794",
    "ffffff",
    "192333",
    "b3b1ad",
    "FFEE99",
    "39bae6",
    "C2D94C",
    "95E6CB",
    "FFB454",
    "FF8F40",
    "F07178",
  }
  local base16 = require "base16"
  base16(base16_ayu, true)
end

M.colors = {
  fg = "#b3b1ad",
  bg = "#0A0E14",
  lightbg = "#192333",
  linebg = "#192333",
  fgfaded = "#626a73",
  grey = "#474e59",
  light_grey = "#707b8c",
  dark_grey = "#131b28",
  darker_grey = "#101621",
  bright = "#ffffff",
  red = "#F07178",
  green = "#B8CC52",
  blue = "#36A3D9",
  yellow = "#FFB454",
  magenta = "#A37ACC",
  orange = "#FF7733",
  cyan = "#80D4FF",
  ViMode = {}
}

M.overrides = function() end

return M
