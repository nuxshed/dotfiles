-- spacegray

local M = {}

function M.base16()
  local base16_spacegray = require("base16").theme_from_array {
    "202020",
    "333333",
    "4f5b66",
    "65737e",
    "65737e",
    "abb2bf",
    "d8dee9",
    "333333",
    "bf616a",
    "d08770",
    "eccc8e",
    "73aa7b",
    "96b5b4",
    "8fa1b3",
    "b48ead",
    "D16969",
  }
  local base16 = require "base16"
  base16(base16_spacegray, true)
end

M.colors = {
  fg = "#abb2bf",
  bg = "#202020",
  lightbg = "#333333",
  linebg = "#333333",
  fgfaded = "#65737e",
  grey = "#595959",
  light_grey = "#7f7f7f",
  dark_grey = "#2b2b2b",
  darker_grey = "#262626",
  bright = "#d8dee9",
  red = "#bf616a",
  green = "#73aa7b",
  blue = "#8fa1b3",
  yellow = "#eccc8e",
  magenta = "#b48ead",
  orange = "#d08770",
  cyan = "#4EC9B0",
}

M.overrides = function() end

return M
