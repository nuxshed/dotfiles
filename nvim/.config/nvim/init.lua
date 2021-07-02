--  _       _ _     _
-- (_)_ __ (_) |_  | |_   _  __ _
-- | | '_ \| | __| | | | | |/ _` |
-- | | | | | | |_ _| | |_| | (_| |
-- |_|_| |_|_|\__(_)_|\__,_|\__,_|

vim.cmd("syntax on")
vim.g.mapleader = " "
vim.g.colorscheme = "gruvdark"

require("plugins")
require("mappings")
require("settings")
require("highlights")
