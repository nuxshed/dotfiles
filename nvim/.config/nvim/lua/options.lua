local fn = vim.fn

vim.opt.showmode = false -- disable mode indicator in cmdline
vim.g.mapleader = O.leader -- set the leader
vim.cmd "syntax on" -- enable syntax highlighting
vim.opt.number = true -- show numbers
vim.opt.numberwidth = 4 -- width of the number column
vim.opt.title = true -- enable window title
vim.opt.titlestring = "nvim" -- set the window title to "nvim"
vim.g.format_on_save = true -- serves no purpose right now

-- timing stuff
vim.opt.updatetime = 300
vim.opt.timeout = true
vim.opt.timeoutlen = 500
vim.opt.ttimeoutlen = 10

-- Window Splitting and Buffers
vim.opt.hidden = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.eadirection = "hor"
-- exclude usetab as we do not want to jump to buffers in already open tabs
-- do not use split or vsplit to ensure we don't open any new windows
vim.o.switchbuf = "useopen,uselast"
vim.opt.fillchars = {
  vert = "│",
  fold = " ",
  eob = " ", -- suppress ~ at EndOfBuffer
  diff = "░", -- alternatives = ⣿ ─
  msgsep = "‾",
  foldopen = "▾",
  foldsep = "│",
  foldclose = "▸",
}

vim.opt.emoji = false -- no emoji
vim.opt.ruler = false -- no ruler
vim.opt.ignorecase = true -- iGnOre CaSe
vim.opt.termguicolors = true -- C O L O R S
vim.opt.cul = true -- cul stands for cursorline
vim.opt.mouse = "a" -- mouse
vim.opt.signcolumn = "yes" -- Always show the signcolumn, we don't want it to shift the text each time
vim.opt.cmdheight = 1 -- height of the cmdline
vim.opt.clipboard = "unnamedplus" -- sync with system clipboard
vim.opt.conceallevel = 2 -- Hide * markup for bold and italic
vim.opt.inccommand = "split" -- preview incremental substitute
vim.opt.joinspaces = false -- No double spaces with join after a dot
vim.opt.list = true -- Show some invisible characters (tabs...
vim.opt.listchars = "tab:> ,nbsp:+,trail:·,extends:→,precedes:←"
vim.opt.expandtab = true -- use spaces when i press tab
vim.opt.shiftwidth = 2 -- width of tab
vim.opt.smartindent = true --sentient indenting
vim.opt.hidden = true -- Enable modified buffers in background
vim.opt.pumblend = 10 -- Popup blend
vim.opt.pumheight = 10 -- Maximum number of entries in a popup
vim.opt.undofile = true
vim.opt.undolevels = 10000
vim.opt.splitbelow = true -- Put new windows below current
vim.opt.splitright = true -- Put new windows right of current
vim.opt.wildmode = "longest:full,full" -- Command-line completion mode
vim.opt.wrap = false -- Disable line wrap
vim.opt.sessionoptions = { "buffers", "curdir", "tabpages", "winsize" }

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
vim.g.markdown_fenced_languages = fences

-- plasticboy/vim-markdown
vim.g.vim_markdown_folding_level = 10
vim.g.vim_markdown_fenced_languages = fences
vim.g.vim_markdown_folding_style_pythonic = 1
vim.g.vim_markdown_conceal_code_blocks = 0
vim.g.vim_markdown_frontmatter = 1
vim.g.vim_markdown_strikethrough = 1

vim.o.grepprg =
  [[rg --hidden --glob "!.git" --no-heading --smart-case --vimgrep --follow $*]]
vim.opt.grepformat = vim.opt.grepformat ^ { "%f:%l:%c:%m" }

vim.g.loaded_gzip = 1
vim.g.loaded_tar = 1
vim.g.loaded_tarPlugin = 1
vim.g.loaded_zipPlugin = 1
vim.g.loaded_2html_plugin = 1
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.g.loaded_matchit = 1
-- vim.g.loaded_matchparen = 1
vim.g.loaded_spec = 1
