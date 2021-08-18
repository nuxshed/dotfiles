-- subliminal

local M = {}

function M.base16()
  local base16_subliminal = require("base16").theme_from_array {
    "282c35",
    "343d46",
    "343d46",
    "7F7F7F",
    "7F7F7F",
    "d4d4d4",
    "d4d4d4",
    "343d46",
    "d4d4d4",
    "91c5d3",
    "f1a5ab",
    "a9cfa4",
    "91c5d3",
    "7F7F7F",
    "7F7F7F",
    "e15a60",
  }
  local base16 = require "base16"
  base16(base16_subliminal, true)
end

M.colors = {
  fg = "#d4d4d4",
  bg = "#22252d",
  lightbg = "#343d46",
  linebg = "#343d46",
  fgfaded = "#7F7F7F",
  grey = "#424959",
  light_grey = "#69738c",
  dark_grey = "#2d323d",
  darker_grey = "#262933",
  bright = "#d4d4d4",
  red = "#e15a60",
  green = "#a9cfa4",
  blue = "#6699CC",
  yellow = "#ffe2a9",
  magenta = "#c594c5",
  orange = "#d19a66",
  cyan = "#5fb3b3",
  ViMode = {},
}

M.overrides = function() end

return M
