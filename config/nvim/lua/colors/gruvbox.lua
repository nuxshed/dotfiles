local M = {}
local cmd = vim.cmd

function M.setup()
  vim.g.background = "dark"
  vim.g.gruvbox_material_background = "hard"
  vim.g.gruvbox_material_sign_column_background = "none"
  vim.g.gruvbox_material_menu_selection_background = "aqua"
  -- vim.g.gruvbox_material_diagnostic_text_highlight = 1
  vim.g.gruvbox_material_diagnostic_virtual_text = "colored"
  vim.g.gruvbox_material_better_performance = 1
  cmd "colorscheme gruvbox-material"
end

M.colors = {
  fg = "#ddc7a1",
  bg = "#1d2021",
  accent = "#89b482",
  lightbg = "#282828",
  fgfaded = "#a89984",
  grey = "#928374",
  light_grey = "#a89984",
  dark_grey = "#383432",
  bright = "#fbf1c7",
  red = "#ea6962",
  green = "#a9b665",
  blue = "#7daea3",
  yellow = "#d8a657",
  magenta = "#d3869b",
  orange = "#e78a4e",
  cyan = "#89b482",
  ViMode = {
    Normal = "#89b482",
  },
}

function M.overrides()
  vim.cmd [[
    hi NormalFloat guibg=#1d2021
    hi TelescopeSelection guibg=#282828 guifg=#89b482 gui=bold
    hi NvimTreeFolderIcon guifg=#7daea3
    hi NvimTreeFolderName guifg=#7daea3
    hi FloatBorder guifg=#89b482
    hi TabLineSel guibg=#282828 guifg=#ddc7a1
    hi TabLineNorm guibg=#1d2021 guifg=#a89984
    hi TabLineFill guibg=#1d2021
    hi TSKeywordFunction gui=italic guifg=#ea6962
    hi TSConditional gui=italic guifg=#ea6962
    hi TSRepeat gui=italic guifg=#ea6962
    hi TelescopeTitle guifg=#928374
    hi TelescopeBorder guifg=#303030
  ]]
end
return M
