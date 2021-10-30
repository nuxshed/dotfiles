vim.g.nvim_tree_icons = {
  default = "",
  symlink = "",
  git = {
    unstaged = "",
    staged = "",
    unmerged = "",
    renamed = "",
    untracked = "",
    deleted = "",
    ignored = "",
  },
}

vim.g.nvim_tree_ignore = {
  ".git",
  ".cache",
  ".node_modules",
}

vim.g.nvim_tree_root_folder_modifier = ":t"
vim.g.nvim_tree_git_hl = 1

require("nvim-tree").setup {
  diagnostics = { enable = true }, -- show lsp diagnostics in the signcolumn
  view = {
    width = 35,
    side = "left",
    hide_root_folder = true,
  },
  hijack_cursor = true,
}

utils.nnoremap("<C-n>", "<CMD>NvimTreeToggle<CR>")
