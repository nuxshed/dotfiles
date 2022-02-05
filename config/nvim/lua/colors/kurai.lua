local M = {}

function M.setup()
  require("base16-colorscheme").setup {
    base00 = "#191513",
    base01 = "#211b19",
    base02 = "#211b19",
    base03 = "#6b655c",
    base04 = "#6b655c",
    base05 = "#898176",
    base06 = "#f5f5f5",
    base07 = "#211b19",
    base08 = "#898176",
    base09 = "#896079",
    base0A = "#896664",
    base0B = "#5a7051",
    base0C = "#898176",
    base0D = "#777a59",
    base0E = "#896664",
    base0F = "#6b655c",
  }
end

M.colors = {
  fg = "#898176",
  bg = "#191513",
  accent = "#898176",
  lightbg = "#211b19",
  fgfaded = "#6b655c",
  grey = "#6b655c",
  light_grey = "#6b655c",
  dark_grey = "#383432",
  bright = "#ffffff",
  red = "#896664",
  green = "#5a7051",
  blue = "#646d89",
  yellow = "#777a59",
  magenta = "#896079",
  orange = "#896664",
  cyan = "#64897a",
}

function M.overrides()
  vim.cmd [[
    hi TabLineSel guibg=#211b19 guifg=#898176
    hi TabLineNorm guibg=#191513 guifg=#6b655c
    hi TabLineFill guibg=#191513 gui=none
    hi TelescopeBorder guifg=#211b19
    hi TelescopeTitle guifg=#6b655c
    hi TSPunctBracket guifg=#898176
    hi TSConstructor guifg=#898176
    hi MatchParen guibg=#423e39
    hi TSKeyword gui=italic
    hi TSConditional gui=italic
    hi TSString gui=italic
  ]]
end
return M
