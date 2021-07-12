-- settings.lua is for settings

local fn = vim.fn

vim.opt.showmode = false -- the mode is shown in the statusline
vim.g.mapleader = O.leader
vim.cmd("syntax on")
vim.opt.number = true
vim.opt.numberwidth = 4
vim.opt.title = true
vim.opt.titlestring = "nvim"

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

vim.opt.emoji = false
vim.opt.ruler = false
vim.opt.ignorecase = true
vim.opt.termguicolors = true
vim.opt.cul = true
vim.opt.mouse = "a"
vim.opt.signcolumn = "yes"
vim.opt.cmdheight = 1
vim.opt.clipboard = "unnamedplus"
vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.smartindent = true

vim.o.grepprg = [[rg --hidden --glob "!.git" --no-heading --smart-case --vimgrep --follow $*]]
vim.opt.grepformat = vim.opt.grepformat ^ { "%f:%l:%c:%m" }
