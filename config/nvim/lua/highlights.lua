require("colors." .. O.colorscheme).setup()
local c = require("colors." .. O.colorscheme).colors

local cmd = vim.cmd
cmd("hi StatusLineAccent guifg=" .. c.bg .. " guibg=" .. c.accent or c.magenta)
cmd("hi StatusLineInsertAccent guifg=" .. c.bg .. " guibg=" .. c.red)
cmd("hi StatusLineVisualAccent guifg=" .. c.bg .. " guibg=" .. c.green)
cmd("hi StatusLineReplaceAccent guifg=" .. c.bg .. " guibg=" .. c.red)
cmd("hi StatusLineCmdLineAccent guifg=" .. c.bg .. " guibg=" .. c.yellow)
cmd("hi StatuslineTerminalAccent guifg=" .. c.bg .. " guibg=" .. c.yellow)
cmd("hi StatusLineExtra guifg=" .. c.fgfaded)
cmd "hi StatusLineNC guibg=NONE"

-- cmd("hi CursorLineNr guibg=" .. c.lightbg)

require("colors." .. O.colorscheme).overrides()
