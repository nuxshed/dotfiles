-- gruvbox dark

local M = {}

function M.base16()
  local base16 = require "base16"
  local base16_gruvbox = require("base16").theme_from_array {
    "1d2021",
    "282828",
    "3c3836",
    "928374",
    "d4be98",
    "ddc7a1",
    "fbf1c7",
    "282828",
    "ddc7a1",
    "89b482",
    "d8a657",
    "a9b665",
    "a9b665",
    "a9b665",
    "ea6962",
    "ea6962",
  }
  base16(base16_gruvbox, true)
  local cmd = vim.cmd
  cmd "hi TSKeyword guifg=#ea6962 gui=italic"
  cmd "hi TSConditional guifg=#ea6962 gui=italic"
  cmd "hi TSTagDelimiter guifg=#a9b665"
  cmd "hi TSKeywordFunction guifg=#89b482"
  cmd "hi TSFunction guifg=#ddc7a1"
  cmd "hi TSRepeat guifg=#ea6962 gui=italic"
  cmd "hi TSOperator guifg=#e78a4e"
  cmd "hi TSTag guifg=#e78a4e"
  cmd "hi bashTSVariable guifg=#7daea3"
  cmd "hi bashTSConstant guifg=#7daea3"
  cmd "hi bashTSPunctSpecial guifg=#7daea3"
  cmd "hi javascriptTSBoolean guifg=#d3869b"
  cmd "hi MatchParen guifg=NONE guibg=#3c3836"
  cmd "hi TSTagAttribute guifg=#89b482"
  cmd "hi pythonTSFuncBuiltin guifg=#d8a657"
  cmd "hi pythonTsNumber guifg=#d3869b"
  cmd "hi rubyMacro guifg=#ea6962 gui=italic"
  cmd "hi rubyControl guifg=#ea6962 gui=italic"
  cmd "hi rubyBlockParameterList guifg=#7daea3"
end

M.colors = {
  fg = "#ddc7a1",
  bg = "#1d2021",
  lightbg = "#282828",
  linebg = "#282828",
  fgfaded = "#a89984",
  grey = "#928374",
  light_grey = "#a89984",
  dark_grey = "#212526",
  darker_grey = "#242424",
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

M.overrides = function() end

return M
