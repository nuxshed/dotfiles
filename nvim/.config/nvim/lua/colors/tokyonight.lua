-- tokyonight
-- this is still a work in progress

local M = {}

function M.base16()
  local base16_tokyonight = require("base16").theme_from_array {
    "1a1b26",
    "24283b",
    "0e434c",
    "565f89",
    "4e5172",
    "c0caf5",
    "dde0ff",
    "24283b",
    "c0caf5",
    "ff9e64",
    "3d59a1",
    "9ece6a",
    "db4b4b",
    "2ac3de",
    "9d7cd8",
    "db4b4b",
  }
  local base16 = require "base16"
  base16(base16_tokyonight, true)
end

M.colors = {
  fg = "#c0caf5",
  bg = "#1a1b26",
  lightbg = "#24283b",
  linebg = "#24283b",
  fgfaded = "#565f89",
  grey = "#4e5172",
  light_grey = "#686c99",
  dark_grey = "#222433",
  darker_grey = "#1d1e2b",
  bright = "#dde0ff",
  red = "#db4b4b",
  green = "#9ece6a",
  blue = "#7aa2f7",
  yellow = "#e0af68",
  magenta = "#9d7cd8",
  orange = "#ff9e64",
  cyan = "#1abc9c",
}

M.overrides = function() end

return M
