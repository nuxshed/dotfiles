vim.g.nvim_tree_width = 30
vim.g.nvim_tree_git_hl = 0
vim.g.nvim_tree_follow = 1

vim.g.nvim_tree_icons = {
  default = "",
  symlink = "",
  git = {
    unstaged = "✗",
    staged = "✓",
    unmerged = "",
    renamed = "➜",
    untracked = "*",
    deleted = "",
    ignored = "x",
  },
  folder = {
    arrow_open = "",
    arrow_closed = "",
    default = "",
    open = "",
    empty = "",
    empty_open = "",
    symlink = "",
    symlink_open = "",
  },
  lsp = {
    hint = "",
    info = "",
    warning = "",
    error = "",
  },
}

vim.api.nvim_set_keymap("n", "<C-n>", ":NvimTreeToggle<CR>", {
  noremap = true,
  silent = true,
})
