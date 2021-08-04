-- seoul

local M = {}

function M.base16()
  local base16_seoul = require("base16").theme_from_array {
    "282828",
    "3a3a3a",
    "005f7f",
    "5f875f",
    "d0d0d0",
    "d0d0d0",
    "e5e5e5",
    "3a3a3a",
    "d0d0d0",
    "ffd787",
    "85add4",
    "87afaf",
    "d8af5f",
    "d7d7af",
    "d75f87",
    "d68787",
  }
  local base16 = require "base16"
  base16(base16_seoul, true)
end

M.colors = {
  fg = "#d0d0d0",
  bg = "#282828",
  lightbg = "#3a3a3a",
  linebg = "#3a3a3a",
  fgfaded = "#626262",
  grey = "#666666",
  light_grey = "#999999",
  dark_grey = "#353535",
  darker_grey = "#303030",
  bright = "#e5e5e5",
  red = "#d68787",
  green = "#87af87",
  blue = "#85add4",
  yellow = "#ffd787",
  magenta = "#d75f87",
  orange = "#d8af5f",
  cyan = "#87d7d7",
}

M.overrides = function() end

return M
