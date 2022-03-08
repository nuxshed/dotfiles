require("gitsigns").setup {
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "-" },
    topdelete = { text = "-" },
    changedelete = { text = "~" },
  },
  signcolumn = false,
  keymaps = {
    ----[[   ]]Default keymap options
    noremap = true,
    buffer = true,

    ["n ]c"] = {
      expr = true,
      "&diff ? ']c' : '<cmd>lua require\"gitsigns.actions\".next_hunk()<CR>'",
    },
    ["n [c"] = {
      expr = true,
      "&diff ? '[c' : '<cmd>lua require\"gitsigns.actions\".prev_hunk()<CR>'",
    },

    ["n <leader>gs"] = '<cmd>lua require"gitsigns".stage_hunk()<CR>',
    ["n <leader>gb"] = '<cmd>lua require"gitsigns".blame_line()<CR>',
    ["v <leader>gs"] = '<cmd>lua require"gitsigns".stage_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
    ["n <leader>gu"] = '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>',
    ["n <leader>gr"] = '<cmd>lua require"gitsigns".reset_hunk()<CR>',
    ["v <leader>gr"] = '<cmd>lua require"gitsigns".reset_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
    ["n <leader>gR"] = '<cmd>lua require"gitsigns".reset_buffer()<CR>',
    ["n <leader>gp"] = '<cmd>lua require"gitsigns".preview_hunk()<CR>',

    -- Text objects
    ["o ih"] = ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>',
    ["x ih"] = ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>',
  },
}
