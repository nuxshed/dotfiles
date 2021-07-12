local colors = require("colors/" .. vim.g.colorscheme).colors
local base16 = require("colors." .. vim.g.colorscheme).base16
local cmd = vim.cmd

-- syntax
-- these are defined here so that the highlights defined below are not overridden by the syntax theme

local function set_syntax_theme()
  if vim.g.colorscheme == "onedark" then
    require("base16")(base16, true)
  elseif vim.g.colorscheme == "nord" then
    require("base16")(base16, true)
  end
end

set_syntax_theme()

--- indentline
cmd("hi IndentBlanklineChar guifg=" .. colors.linebg)
cmd("hi IndentBlanklineContextChar guifg=" .. colors.grey)

-- misc
cmd("hi LineNr guifg=" .. colors.grey)
cmd("hi Comment guifg=" .. colors.fgfaded)
cmd("hi NvimInternalError guifg=" .. colors.red)
cmd("hi EndOfBuffer guifg=" .. colors.bg)
-- cmd("hi! StatusLineNC gui=underline guifg=" .. colors.linebg)
cmd("hi StatusLine guibg=" .. colors.bg .. " guifg=" .. colors.bg)

-- underline all uses of the word under the cursor
cmd("hi LspReferenceRead gui=underline")
cmd("hi LspReferenceWrite gui=underline")

-- popup menu
cmd("hi Pmenu guibg=" .. colors.dark_grey)
cmd("hi PmenuSbar guibg=" .. colors.dark_grey)
cmd("hi PmenuSel guibg=" .. colors.green)
cmd("hi PmenuThumb guibg=" .. colors.grey)

-- git signs
-- this doesn't work when guibg is specified before/at the same time as guifg
cmd("hi GitSignsAdd guibg=" .. colors.bg)
cmd("hi GitSignsAdd guifg=" .. colors.green)
cmd("hi GitSignsChange guibg=" .. colors.bg)
cmd("hi GitSignsChange guifg=" .. colors.yellow)
cmd("hi GitSignsDelete guibg=" .. colors.bg)
cmd("hi GitSignsDelete guifg=" .. colors.red)

-- highlight current line and number
cmd("hi CursorLine guibg=" .. colors.darker_grey)
cmd("hi cursorlinenr guifg=" .. colors.fg)

-- NvimTree
cmd("hi NvimTreeFolderIcon guifg=" .. colors.blue)
cmd("hi NvimTreeFolderName guifg=" .. colors.blue)
cmd("hi NvimTreeIndentMarker guifg=" .. colors.lightbg)
cmd("hi NvimTreeVertSplit guifg=" .. colors.bg .. " guibg=" .. colors.bg)
cmd("hi NvimTreeRootFolder guifg=" .. colors.red)
cmd("hi NvimTreeNormal guibg=" .. colors.bg)
cmd("hi NvimTreeStatusLine guifg=" .. colors.bg .. " guibg=" .. colors.bg)

-- telescope
cmd("hi TelescopeBorder guifg=" .. colors.linebg)
cmd("hi TelescopePromptBorder guifg=" .. colors.linebg)
cmd("hi TelescopeResultsBorder guifg=" .. colors.linebg)
cmd("hi TelescopeResultsBorder guifg=" .. colors.linebg)
cmd("hi TelescopePromptBorder guifg=" .. colors.grey)

-- Lsp Stuff

-- error / warnings
cmd("hi LspDiagnosticsSignError guifg=" .. colors.red)
cmd("hi LspDiagnosticsVirtualTextError guifg=" .. colors.red)
cmd("hi LspDiagnosticsDefaultError guifg=" .. colors.red)
cmd("hi LspDiagnosticsSignWarning guifg=" .. colors.yellow)
cmd("hi LspDiagnosticsVirtualTextWarning guifg=" .. colors.yellow)
cmd("hi LspDiagnosticsDefaultWarning guifg=" .. colors.yellow)

-- info
cmd("hi LspDiagnosticsSignInformation guifg=" .. colors.green)
cmd("hi LspDiagnosticsVirtualTextInformation guifg=" .. colors.green)
cmd("hi LspDiagnosticsDefaultInformation guifg=" .. colors.green)

-- hint
cmd("hi LspDiagnosticsSignHint guifg=" .. colors.magenta)
cmd("hi LspDiagnosticsVirtualTextHint guifg=" .. colors.magenta)
cmd("hi LspDiagnosticsDefaultHint guifg=" .. colors.magenta)

-- the background color for neovim
cmd("hi Normal guibg=" .. colors.bg)
