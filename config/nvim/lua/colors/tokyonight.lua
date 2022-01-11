-- tokyonight

local M = {}

function M.setup()
  vim.g.tokyonight_style = "storm"
  vim.cmd "colorscheme tokyonight"
end

M.colors = {
  fg = "#c0caf5",
  bg = "#1a1b26",
  accent = "#7aa2f7",
  lightbg = "#24283b",
  fgfaded = "#565f89",
  grey = "#4e5172",
  light_grey = "#686c99",
  dark_grey = "#222433",
  bright = "#dde0ff",
  red = "#db4b4b",
  green = "#9ece6a",
  blue = "#7aa2f7",
  yellow = "#e0af68",
  magenta = "#9d7cd8",
  orange = "#ff9e64",
  cyan = "#1abc9c",
  ViMode = {},
}

M.overrides = function()
  vim.cmd [[
    hi Green guibg=NONE guifg=#9ece6a
    hi TabLineSel guibg=#292e42 guifg=#c0caf5
    hi TabLineNorm guibg=NONE guifg=#565f89
    hi TabLineFill guibg=NONE
    hi TelescopeBorder guifg=#292e42
    hi TelescopeTitle guifg=#565f89
    hi TelescopeSelection guibg=#292e42 guifg=#7aa2f7
  ]]
end

return M
