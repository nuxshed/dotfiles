local M = {}
local cmd = vim.cmd

function M.setup()
  vim.g.zenbones_compat = true
  cmd "colorscheme zenbones"
  cmd "set background=light"
end

M.colors = {
  fg = "#26363C",
  bg = "#F0EDEC",
  accent = "#26363c",
  lightbg = "#e9e4e2",
  fgfaded = "#948985",
  grey = "#948985",
  light_grey = "#948985",
  dark_grey = "#383432",
  bright = "#ffffff",
  red = "#A8334C",
  green = "#4F6C31",
  blue = "#286486",
  yellow = "#944927",
  magenta = "#88507D",
  orange = "#803D1C",
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
  ]]
end
return M
