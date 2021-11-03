local M = {}
local cmd = vim.cmd

-- setup colors
function M.setup()
  require("base16-colorscheme").setup {
    base00 = "#f7f7f7",
    base01 = "#e3e3e3",
    base02 = "#cccccc",
    base03 = "#ababab",
    base04 = "#525252",
    base05 = "#4c4c4c",
    base06 = "#252525",
    base07 = "#cccccc",
    base08 = "#7c7c7c",
    base09 = "#999999",
    base0A = "#666666",
    base0B = "#8e8e8e",
    base0C = "#868686",
    base0D = "#686868",
    base0E = "#747474",
    base0F = "#5e5e5e",
  }
end

M.colors = {
  fg = "#000000",
  bg = "#4c4c4c",
  accent = "#cccccc",
  lightbg = "#cccccc",
  fgfaded = "#999999",
  grey = "#000000",
  light_grey = "#000000",
  dark_grey = "#000000",
  bright = "#000000",
  red = "#cccccc",
  green = "#cccccc",
  blue = "#cccccc",
  yellow = "#cccccc",
  magenta = "#cccccc",
  orange = "#cccccc",
  cyan = "#cccccc",
}

function M.overrides()
  cmd [[
    hi TabLineNorm guibg=NONE guifg=#999999
    hi TabLineSel guibg=#e3e3e3 guifg=#555555
    hi TabLineFill guibg=NONE
    hi LspReferenceText gui=NONE guibg=#e5e5e5
    hi LspReferenceWrite gui=NONE guibg=#e5e5e5
    hi LspReferenceRead gui=NONE guibg=#e5e5e5
    hi TSKeyword gui=italic
    hi TSKeywordFunction gui=italic
    hi TSRepeat gui=italic
    hi TSKeywordOperator gui=italic
    hi TSConditional gui=italic
    hi TSProperty gui=italic
    hi MatchParen guibg=#cccccc
    hi TelescopeBorder guifg=#ababab
    hi TelescopeTitle guifg=#4c4c4c
  ]]
end
return M
