require("toggleterm").setup {
  size = function(term)
    if term.direction == "horizontal" then
      return 15
    elseif term.direction == "vertical" then
      return vim.o.columns / 2
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
