-- serenade
-- WIP, syntax higlighting is different from original

local M = {}

function M.base16()
  local base16_serenade = require("base16").theme_from_array {
    "191c1e",
    "2a2f33",
    "474F54",
    "474F54",
    "a1a8af",
    "bfddb2",
    "ddffce",
    "2a2f33",
    "bfddb2",
    "d76e6e",
    "82abbc",
    "ACB765",
    "a1a8af",
    "ACB765",
    "d76e6e",
    "d76e6e",
  }
  local base16 = require "base16"
  base16(base16_serenade, true)
end

M.colors = {
  fg = "#bfddb2",
  bg = "#191c1e",
  lightbg = "#2a2f33",
  linebg = "#2a2f33",
  fgfaded = "#474F54",
  grey = "#7f868c",
  light_grey = "#a1a8af",
  dark_grey = "#1f2326",
  darker_grey = "#191c1e",
  bright = "#ddffce",
  red = "#d76e6e",
  green = "#ACB765",
  blue = "#82abbc",
  yellow = "#c1bf89",
  magenta = "#d39bb6",
  orange = "#e5a46b",
  cyan = "#87c095",
}
return M
