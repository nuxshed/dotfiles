local nvim_tree = require("nvim-tree")

nvim_tree.setup({
    disable_netrw = true,
    hijack_netrw = true,
    update_cwd = true,
    diagnostics = {
        enable = true,
        icons = {
            hint = "",
            info = "",
            warning = "",
            error = "",
        },
    },
    update_focused_file = {
        enable = true,
        update_cwd = true,
        ignore_list = {},
    },
})

vim.g.nvim_tree_ignore = { ".git", "node_modules", ".cache" }
