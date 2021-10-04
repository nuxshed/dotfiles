-- serenade

local M = {}

function M.setup()
  vim.g.serenade_sign_column_background = "none"
  vim.g.serenade_better_performance = 1
  vim.cmd [[colorscheme serenade]]
end

M.colors = {
  fg = "#bfddb2",
  bg = "#2A2F33",
  accent = "#ACB765",
  lightbg = "#343b3f",
  linebg = "#343b3f",
  fgfaded = "#474F54",
  grey = "#7f868c",
  light_grey = "#a1a8af",
  dark_grey = "#2c3135",
  bright = "#ddffce",
  red = "#d76e6e",
  green = "#ACB765",
  blue = "#82abbc",
  yellow = "#c1bf89",
  magenta = "#d39bb6",
  orange = "#e5a46b",
  cyan = "#87c095",
  ViMode = {},
}

M.overrides = function()
  vim.cmd [[
    hi TabLineSel guibg=#343b3f guifg=NONE
    hi TabLineNorm guibg=NONE guifg=#a1a8af
    hi TabLineFill guibg=NONE
    hi Statusline guifg=#bfddb2 guibg=#343b3f
    hi DiagnosticError guifg=#d76e6e
    hi DiagnosticWarn guifg=#c1bf89
    hi DiagnosticInfo guifg=#82abbc
    hi DiagnosticHint guifg=#d39bb6
    hi DiagnosticUnderlineError guisp=#d76e6e gui=underline
    hi DiagnosticUnderlineWarn guisp=#c1bf89 gui=underline
    hi DiagnosticUnderlineInfo guisp=#82abbc gui=underline
    hi DiagnosticUnderlineHint guisp=#d39bb6 gui=underline
  ]]
end

return M
