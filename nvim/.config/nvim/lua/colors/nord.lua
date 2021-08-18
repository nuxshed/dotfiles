-- nord

local M = {}

function M.base16()
  local base16 = require "base16"
  local base16_nord = base16.theme_from_array {
    "21252d",
    "333a47",
    "434c5e",
    "616e88",
    "616e88",
    "d8dee9",
    "eceff4",
    "333a47",
    "d8dee9",
    "ebcb8b",
    "8fbcbb",
    "a3be8c",
    "ebcb8b",
    "81a1c1",
    "667499",
    "bf616a",
  }
  base16(base16_nord, true)
end

M.colors = {
  fg = "#d8dee9",
  bg = "#21252d",
  lightbg = "#333a47",
  linebg = "#333a47",
  fgfaded = "#616e88",
  grey = "#3e4656",
  light_grey = "#4c566a",
  dark_grey = "#282d38",
  darker_grey = "#242933",
  bright = "#eceff4",
  red = "#bf616a",
  green = "#a3be8c",
  blue = "#8fbcbb",
  yellow = "#ebcb8b",
  magenta = "#b48ead",
  orange = "#d08770",
  cyan = "#9fe2f4",
  ViMode = {},
}

M.overrides = function() end

return M
