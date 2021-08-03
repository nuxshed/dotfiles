-- serenade
-- WIP, syntax higlighting is different from original

local M = {}

function M.base16()
  local base16_serenade = require("base16").theme_from_array {
    "23282b",
    "343b3f",
    "474F54",
    "474F54",
    "a1a8af",
    "bfddb2",
    "ddffce",
    "343b3f",
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
  bg = "#23282b",
  lightbg = "#343b3f",
  linebg = "#343b3f",
  fgfaded = "#474F54",
  grey = "#7f868c",
  light_grey = "#a1a8af",
  dark_grey = "#2c3135",
  darker_grey = "#282d30",
  bright = "#ddffce",
  red = "#d76e6e",
  green = "#ACB765",
  blue = "#82abbc",
  yellow = "#c1bf89",
  magenta = "#d39bb6",
  orange = "#e5a46b",
  cyan = "#87c095",
}

M.overrides = function() end

return M
