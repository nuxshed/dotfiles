-- doom one
-- mostly identical to onedark, except syntax highlighting

local M = {}

function M.base16()
  local base16_onedark = require("base16").theme_from_array {
    "131519",
    "282c34",
    "2257A0",
    "5B6268",
    "898f99",
    "abb2bf",
    "DFDFDF",
    "282c34",
    "c678dd",
    "da8548",
    "ECBE7B", -- *
    "98be65",
    "98be65", -- *
    "c678dd",
    "51afef",
    "ff6c6b",
  }
  local base16 = require "base16"
  base16(base16_onedark, true)
end

M.colors = {
  fg = "#abb2bf",
  bg = "#131519",
  lightbg = "#282c34",
  linebg = "#282c34",
  fgfaded = "#5B6268",
  grey = "#3f444a",
  light_grey = "#73797e",
  dark_grey = "#202328",
  darker_grey = "#1c1f24",
  bright = "#DFDFDF",
  red = "#ff6c6b",
  green = "#98be65",
  blue = "#51afef",
  yellow = "#ECBE7B",
  magenta = "#c678dd",
  orange = "#da8548",
  cyan = "#46D9FF",
}

M.overrides = function() end

return M
