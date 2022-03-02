local M = {}

function M.setup()
  require("base16-colorscheme").setup {
    base00 = "#F0EDEC",
    base01 = "#e9e4e2",
    base02 = "#cbd9e3",
    base03 = "#948985",
    base04 = "#8e817b",
    base05 = "#685c56",
    base06 = "#ffffff",
    base07 = "#e9e4e2",
    base08 = "#685c56",
    base09 = "#88507d",
    base0A = "#A8334C",
    base0B = "#597a37",
    base0C = "#685c56",
    base0D = "#a8623e",
    base0E = "#A8334C",
    base0F = "#4f5e68",
  }
end

M.colors = {
  fg = "#685c56",
  bg = "#F0EDEC",
  accent = "#685c56",
  lightbg = "#e9e4e2",
  fgfaded = "#948985",
  grey = "#948985",
  light_grey = "#948985",
  dark_grey = "#383432",
  bright = "#ffffff",
  red = "#A8334C",
  green = "#597a37",
  blue = "#286486",
  yellow = "#a8623e",
  magenta = "#88507D",
  orange = "#944927",
  cyan = "#3B8992",
  ViMode = {
    Normal = "#26363c",
  },
}

function M.overrides()
  vim.cmd [[
    hi TabLineSel guibg=#e9e4e2 guifg=#26363c
    hi TabLineNorm guibg=#F0EDEC guifg=#948985
    hi TabLineFill guibg=#F0EDEC gui=none
    hi TelescopeBorder guifg=#e9e4e2
    hi TelescopeTitle guifg=#948985
    hi TSPunctBracket guifg=#685c56
    hi TSConstructor guifg=#685c56
    hi MatchParen guibg=#e5d9d7
    hi TSKeyword gui=italic
    hi TSConditional gui=italic
    hi TSString gui=italic
  ]]
end
return M
