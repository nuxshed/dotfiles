-- everforest

local M = {}

function M.base16()
  local base16_everforest = require("base16").theme_from_array {
    -- from: https://gist.github.com/sainnhe/3298202006955aab02c77fb01c279298
    "2b3339",
    "323c41",
    "503946",
    "868d80",
    "d3c6aa",
    "d3c6aa",
    "e9e8d2",
    "fff9e8",
    "7fbbb3",
    "d699b6",
    "83c092",
    "dbbc7f",
    "e69875",
    "a7c080",
    "e67e80",
    "d699b6",
  }
  local base16 = require "base16"
  base16(base16_everforest, true)
end

M.colors = {
  fg = "#d3c6aa",
  bg = "#1c2226",
  lightbg = "#2b3339",
  linebg = "#2b3339",
  fgfaded = "#859289",
  grey = "#445055",
  light_grey = "#9aa79d",
  dark_grey = "#21292d",
  darker_grey = "#1e2429",
  bright = "#ffffff",
  red = "#e67e80",
  green = "#a7c080",
  blue = "#7fbbb3",
  yellow = "#dbbc7f",
  magenta = "#d699b6",
  orange = "#e69875",
  cyan = "#7fbbb3",
}

M.overrides = function() end

return M
