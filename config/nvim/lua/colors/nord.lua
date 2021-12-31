-- nord

local M = {}

function M.setup()
  vim.cmd [[colorscheme nord]]
end

M.colors = {
  fg = "#d8dee9",
  bg = "#21252d",
  accent = "#8fbcbb",
  lightbg = "#333a47",
  linebg = "#333a47",
  fgfaded = "#616e88",
  grey = "#3e4656",
  light_grey = "#4c566a",
  dark_grey = "#282d38",
  darker_grey = "#242933",
  bright = "#eceff4",
  red = "#bf616a",
  green = "#a3be8c",
  blue = "#8fbcbb",
  yellow = "#ebcb8b",
  magenta = "#b48ead",
  orange = "#d08770",
  cyan = "#9fe2f4",
  ViMode = {
    Normal = "#8fbcbb",
  },
}

M.overrides = function()
  vim.cmd [[
    hi TabLineSel guifg=#d8dee9 guibg=#393f4a gui=italic
    hi TabLineNorm guifg=#616e88 guibg=#2e3440
    hi TabLineFill guibg=#2e3440
    hi LspReferenceText guibg=#3a404c gui=NONE
    hi LspReferenceWrite guibg=#3a404c gui=NONE
    hi LspReferenceRead guibg=#3a404c gui=NONE
    hi MatchParen guibg=#3a404c
    hi IndentBLanklineContextChar guifg=#61afef
  ]]
end

return M
