require("telescope").setup {
  defaults = {
    prompt_prefix = "> ",
    selection_caret = "> ",
    entry_prefix = "  ",
    initial_mode = "insert",
    selection_strategy = "reset",
    sorting_strategy = "descending",
    layout_strategy = "horizontal",
    layout_config = {
      horizontal = {
        mirror = false,
        preview_width = 0.6,
      },
      vertical = {
        mirror = false,
      },
    },
    file_sorter = require("telescope.sorters").get_fuzzy_file,
    file_ignore_patterns = {},
    generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
    winblend = 0,
    border = {},
    borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
    color_devicons = true,
    use_less = true,
    set_env = { ["COLORTERM"] = "truecolor" }, -- default = nil,
    file_previewer = require("telescope.previewers").vim_buffer_cat.new,
    grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
    qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,

    -- Developer configurations: Not meant for general override
    buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
  },
}

-- mappings

require("which-key").register {
  ["<leader>f"] = {
    name = "+find",
    f = { [[<Cmd>Telescope find_files<CR>]], "Find File" },
    r = { [[<Cmd>Telescope oldfiles<CR>]], "Recent Files" },
    b = { [[<Cmd>Telescope buffers<CR>]], "Find Buffer" },
    w = { [[<Cmd>Telescope live_grep<CR>]], "Find Word" },
    g = {
      name = "+Telescope Git Stuff",
      c = { [[<Cmd>Telescope git_commits<CR>]], "View Git Commits" },
      f = { [[<Cmd>Telescope git_files<CR>]], "View Git Files" },
      b = { [[<Cmd>Telescope git_branches<CR>]], "View Git Branches" },
      s = { [[<Cmd>Telescope git_status<CR>]], "View Git Status" },
    },
  },
}
