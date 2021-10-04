-- onedark(er)

local M = {}

function M.setup()
  require("onedark").setup()
end

M.colors = {
  fg = "#abb2bf",
  bg = "#131519",
  accent = "#c678dd",
  lightbg = "#282c34",
  linebg = "#282c34",
  fgfaded = "#545862",
  grey = "#42464e",
  light_grey = "#6f737b",
  dark_grey = "#1b1d23",
  bright = "#c8ccd4",
  red = "#e06c75",
  green = "#98c379",
  blue = "#61afef",
  yellow = "#e5c07b",
  magenta = "#c678dd",
  orange = "#d19a66",
  cyan = "#56b6c2",
  ViMode = {},
}

M.overrides = function()
  vim.cmd [[
    hi TabLineSel guifg=#abb2bf guibg=#393f4a gui=italic
    hi TabLineNorm guifg=#abbb2bf guibg=#282c34
    hi TabLineFill guibg=#282c34
    hi LspReferenceText guibg=#3a404c gui=NONE
    hi LspReferenceWrite guibg=#3a404c gui=NONE
    hi LspReferenceRead guibg=#3a404c gui=NONE
    hi MatchParen guibg=#3a404c
    hi IndentBLanklineContextChar guifg=#61afef
  ]]
end

return M
