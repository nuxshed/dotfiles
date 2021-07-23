-- nord

local M = {}

M.base16 = require("base16").theme_from_array {
  "2E3440",
  "3B4252",
  "434C5E",
  "4C566A",
  "D8DEE9",
  "E5E9F0",
  "ECEFF4",
  "8FBCBB",
  "88C0D0",
  "81A1C1",
  "5E81AC",
  "BF616A",
  "D08770",
  "EBCB8B",
  "A3BE8C",
  "B48EAD",
}

M.colors = {
  fg = "#e5e9f0",
  bg = "#1b1d23",
  lightbg = "#434c5e",
  linebg = "#434c5e",
  fgfaded = "#545862",
  grey = "#4c566a",
  light_grey = "#6f737b",
  dark_grey = "#3b4252",
  bright = "#eceff4",
  red = "#bf616a",
  green = "#a3be8c",
  blue = "#81a1c1",
  yellow = "#ebcb8b",
  magenta = "#b48ead",
  orange = "#d08770",
  cyan = "#88c0d0",
}
return M
