require("toggleterm").setup {
  size = function(term)
    if term.direction == "horizontal" then
      return 15
    elseif term.direction == "vertical" then
      return vim.o.columns
    end
  end,
  hide_numbers = true,
  open_mapping = [[<M-`>]],
  shade_filetypes = {},
  start_in_insert = true,
  persist_size = true,
  direction = O.terminal.direction,
}

-- Esc twice to get to normal mode
vim.cmd [[tnoremap <esc><esc> <C-\><C-N>]]

local Terminal = require("toggleterm.terminal").Terminal
local lazygit = Terminal:new {
  cmd = "lazygit",
  dir = "git_dir",
  direction = "float",
  float_opts = {
    border = "single",
  },
  on_open = function(term)
    vim.cmd "startinsert!"
    vim.api.nvim_buf_set_keymap(
      term.bufnr,
      "n",
      "q",
      "<cmd>close<CR>",
      { noremap = true, silent = true }
    )
  end,
}

function _lazygit_toggle()
  lazygit:toggle()
end

-- vim.api.nvim_set_keymap("n", "<leader>g", "<cmd>lua _lazygit_toggle()<CR>", { noremap = true, silent = true })
require("utils").map("n", "<leader>g", [[ <Cmd> lua _lazygit_toggle()<CR>]])
