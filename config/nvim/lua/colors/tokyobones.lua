local M = {}
local cmd = vim.cmd

function M.setup()
  vim.g.zenbones_compat = true
  cmd "colorscheme tokyobones"
  cmd "set background=light"
end

M.colors = {
  fg = "#343b58",
  bg = "#eaecf2",
  accent = "#343b58",
  lightbg = "#dedfe5",
  fgfaded = "#7c7e89",
  grey = "#7c7e89",
  light_grey = "#7c7e89",
  dark_grey = "#383432",
  bright = "#ffffff",
  red = "#8c4351",
  green = "#33635c",
  blue = "#34548a",
  yellow = "#8f5e15",
  magenta = "#5a4a78",
  orange = "#965027",
  cyan = "#166775",
  ViMode = {
    Normal = "#343b58",
  },
}

function M.overrides()
  vim.cmd [[
    hi TabLineSel guibg=#dedfe5 guifg=#343b58
    hi TabLineNorm guibg=#d5d6db guifg=#7c7e89
    hi TabLineFill guibg=NONE gui=NONE
    hi TelescopeBorder guifg=#7e7e89
    hi TelescopeTitle guifg=#7c7e89
    hi Normal guibg=#eaecf2
    hi CursorLine guibg=#dedfe5
  ]]
end
return M
