local g = vim.g
local o = vim.opt

o.showmode = false -- disable mode indicator in cmdline
g.mapleader = O.leader -- set the leader
g.maplocalleader = O.localleader -- set the localleader
o.number = true -- show numbers
o.numberwidth = 4 -- width of the number column
o.title = true -- enable window title
o.titlestring = "%f - nvim" -- set the window title to "nvim"

-- timing stuff
o.updatetime = 300
o.timeout = true
o.timeoutlen = 500
o.ttimeoutlen = 10

-- Window Splitting and Buffers
o.hidden = true
o.splitbelow = true
o.splitright = true
o.eadirection = "hor"
-- exclude usetab as we do not want to jump to buffers in already open tabs
-- do not use split or vsplit to ensure we don't open any new windows
o.switchbuf = "useopen,uselast"
o.fillchars = {
  vert = " ",
  fold = " ",
  eob = " ",
  diff = "░",
  msgsep = "‾",
  foldopen = "▾",
  foldsep = "|",
  foldclose = "▸",
}

o.foldmethod = "manual"

o.emoji = false -- no emoji
o.ruler = false -- no ruler
o.ignorecase = true -- iGnOre CaSe
o.termguicolors = true -- C O L O R S
o.cul = true -- cul stands for cursorline
o.mouse = "a" -- mouse
o.signcolumn = "yes" -- Always show the signcolumn, we don't want it to shift the text each time
o.cmdheight = 1 -- height of the cmdline
o.clipboard = "unnamedplus" -- sync with system clipboard
o.conceallevel = 2 -- Hide * markup for bold and italic
o.inccommand = "split" -- preview incremental substitute
o.joinspaces = false -- No double spaces with join after a dot
o.list = true -- Show some invisible characters (tabs...
o.listchars = {
  tab = "| ",
  trail = "·",
}
o.expandtab = true -- use spaces when i press tab
o.shiftwidth = 2 -- width of tab
o.smartindent = true --sentient indenting
o.hidden = true -- Enable modified buffers in background
o.pumblend = 10 -- Popup blend
o.pumheight = 10 -- Maximum number of entries in a popup
o.undofile = true
o.undolevels = 10000
o.splitbelow = true -- Put new windows below current
o.splitright = true -- Put new windows right of current
o.wildmode = "longest:full,full" -- Command-line completion mode
o.wrap = false -- Disable line wrap
o.sessionoptions = { "buffers", "curdir", "tabpages", "winsize" }

-- markdown
-- Use proper syntax highlighting in code blocks
local fences = {
  "lua",
  "vim",
  "json",
  "typescript",
  "javascript",
  "js=javascript",
  "ts=typescript",
  "shell=sh",
  "python",
  "sh",
  "console=sh",
}
g.markdown_fenced_languages = fences

-- plasticboy/vim-markdown
g.vim_markdown_folding_level = 10
g.vim_markdown_fenced_languages = fences
g.vim_markdown_folding_style_pythonic = 1
g.vim_markdown_conceal_code_blocks = 0
g.vim_markdown_frontmatter = 1
g.vim_markdown_strikethrough = 1

o.grepprg = [[rg --hidden --glob "!.git" --no-heading --smart-case --vimgrep --follow $*]]
o.grepformat = vim.opt.grepformat ^ { "%f:%l:%c:%m" }

-- disable inbuilt vim plugins
g.loaded_gzip = 1
g.loaded_tar = 1
g.loaded_tarPlugin = 1
g.loaded_zipPlugin = 1
g.loaded_2html_plugin = 1
g.loaded_netrw = 1
g.loaded_netrwPlugin = 1
g.loaded_matchit = 1
g.loaded_spec = 1
