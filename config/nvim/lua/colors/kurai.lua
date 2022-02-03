local M = {}

function M.setup()
  require("base16-colorscheme").setup {
    base00 = "#2a2320",
    base01 = "#332b26",
    base02 = "#332b26",
    base03 = "#6b655c",
    base04 = "#6b655c",
    base05 = "#b4a99b",
    base06 = "#f5f5f5",
    base07 = "#332b26",
    base08 = "#b4a99b",
    base09 = "#a3728f",
    base0A = "#a37a77",
    base0B = "#84a377",
    base0C = "#b4a99b",
    base0D = "#9fa377",
    base0E = "#a37a77",
    base0F = "#6b655c",
  }
end

M.colors = {
  fg = "#b4a99b",
  bg = "#2a2320",
  accent = "#b4a99b",
  lightbg = "#332b26",
  fgfaded = "#6b655c",
  grey = "#6b655c",
  light_grey = "#6b655c",
  dark_grey = "#383432",
  bright = "#ffffff",
  red = "#a37a77",
  green = "#84a377",
  blue = "#7782a3",
  yellow = "#9fa377",
  magenta = "#a3728f",
  orange = "#a37a77",
  cyan = "#7798a3",
}

function M.overrides()
  vim.cmd [[
    hi TabLineSel guibg=#332b26 guifg=#b4a99b
    hi TabLineNorm guibg=#2a2320 guifg=#6b655c
    hi TabLineFill guibg=#2a2320 gui=none
    hi TelescopeBorder guifg=#332b26
    hi TelescopeTitle guifg=#6b655c
    hi TSPunctBracket guifg=#b4a99b
    hi TSConstructor guifg=#b4a99b
    hi MatchParen guibg=#423e39
    hi TSKeyword gui=italic
    hi TSConditional gui=italic
    hi TSString gui=italic
  ]]
end
return M
