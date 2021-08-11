-- gruvbox dark

local M = {}

function M.base16()
  local base16 = require "base16"
  local base16_gruvbox = require("base16").theme_from_array {
    "1d2021",
    "282828",
    "3c3836",
    "928374",
    "d4be98",
    "ddc7a1",
    "fbf1c7",
    "282828",
    "ddc7a1",
    "89b482",
    "d8a657",
    "89b482",
    "a9b665",
    "a9b665",
    "ea6962",
    "ea6962",
  }
  base16(base16_gruvbox, true)
  vim.cmd "hi TSTagDelimiter guifg=#a9b665"
  vim.cmd "hi TSKeywordFunction guifg=#89b482"
end

M.colors = {
  fg = "#ddc7a1",
  bg = "#1d2021",
  lightbg = "#282828",
  linebg = "#282828",
  fgfaded = "#a89984",
  grey = "#928374",
  light_grey = "#bdae93",
  dark_grey = "#212526",
  darker_grey = "#1f2223",
  bright = "#fbf1c7",
  red = "#ea6962",
  green = "#a9b665",
  blue = "#7daea3",
  yellow = "#d8a657",
  magenta = "#d3869b",
  orange = "#e78a4e",
  cyan = "#89b482",
}

M.overrides = function() end

return M
