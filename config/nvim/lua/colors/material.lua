-- Material Dark (from base16)

local M = {}

function M.setup()
  require("base16-colorscheme").setup {
    base00 = "#263238",
    base01 = "#2E3C43",
    base02 = "#314549",
    base03 = "#546E7A",
    base04 = "#B2CCD6",
    base05 = "#EEFFFF",
    base06 = "#EEFFFF",
    base07 = "#FFFFFF",
    base08 = "#F07178",
    base09 = "#F78C6C",
    base0A = "#FFCB6B",
    base0B = "#C3E88D",
    base0C = "#89DDFF",
    base0D = "#82AAFF",
    base0E = "#C792EA",
    base0F = "#FF5370",
  }
end

M.colors = {
  fg = "#EEFFFF",
  bg = "#131519",
  accent = "#F07178",
  lightbg = "#263238",
  linebg = "#263238",
  fgfaded = "#546E7A",
  grey = "#42464e",
  light_grey = "#6f737b",
  dark_grey = "#1b1d23",
  bright = "#FFFFFF",
  red = "#F07178",
  green = "#C3E88D",
  blue = "#82AAFF",
  yellow = "#FFCB6B",
  magenta = "#C792EA",
  orange = "#d19a66",
  cyan = "#89DDFF",
  ViMode = {},
}

M.overrides = function()
  vim.cmd [[
    hi TabLineSel guifg=#EEFFFF guibg=#393f4a gui=italic
    hi TabLineNorm guifg=#abbb2bf guibg=#263238
    hi TabLineFill guibg=#263238
    hi LspReferenceText guibg=#3a404c gui=NONE
    hi LspReferenceWrite guibg=#3a404c gui=NONE
    hi LspReferenceRead guibg=#3a404c gui=NONE
    hi MatchParen guibg=#3a404c
    hi IndentBLanklineContextChar guifg=#C3E88D
    hi Statusline guifg=NONE
  ]]
end

return M
