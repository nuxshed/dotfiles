require("toggleterm").setup({
  size = 20,
  hide_numbers = true,
  open_mapping = [[<M-`>]],
  shade_filetypes = {},
  start_in_insert = true,
  persist_size = true,
  direction = O.terminal.direction,
})

-- Esc twice to get to normal mode
vim.cmd([[tnoremap <esc><esc> <C-\><C-N>]])
