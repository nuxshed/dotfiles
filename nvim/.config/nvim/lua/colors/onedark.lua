-- onedark(er)

local M = {}

M.base16 = require("base16").theme_from_array({
  "131519",
  "1b1d23",
  "3a404c",
  "545862",
  "565c64",
  "abb2bf",
  "b6bdca",
  "c8ccd4",
  "e06c75",
  "d19a66",
  "e5c07b",
  "98c379",
  "56b6c2",
  "61afef",
  "c678dd",
  "be5046",
})

M.colors = {
  fg = "#abb2bf",
  bg = "#131519",
  lightbg = "#23262d",
  linebg = "#282c34",
  fgfaded = "#545862",
  grey = "#42464e",
  light_grey = "#6f737b",
  dark_grey = "#1b1d23",
  darker_grey = "#17191e",
  bright = "#c8ccd4",
  red = "#e06c75",
  green = "#98c379",
  blue = "#61afef",
  yellow = "#e5c07b",
  magenta = "#c678dd",
  orange = "#d19a66",
  cyan = "#56b6c2",
}
return M
