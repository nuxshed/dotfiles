local M = {}
local cmd = vim.cmd

-- setup colors
function M.setup()
  vim.cmd [[
        colorscheme no-clown-fiesta
    ]]
end

M.colors = {
  fg = "#E1E1E1",
  bg = "#151515",
  accent = "#88afa2",
  lightbg = "#171717",
  fgfaded = "#AFAFAF",
  grey = "#373737",
  light_grey = "#727272",
  dark_grey = "#151515",
  bright = "#E1E1E1",
  red = "#b46958",
  green = "#90A959",
  blue = "#BAD7FF",
  yellow = "#F4BF75",
  magenta = "#AA759F",
  orange = "#FFA557",
  cyan = "#88afa2",
}

function M.overrides()
  cmd [[
        hi Normal guibg=#151515 guifg=#E1E1E1
        hi CursorLine guibg=#191919
        hi AlphaHeader guifg=#F4BF75
    ]]
end

return M
