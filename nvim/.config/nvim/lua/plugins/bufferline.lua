local colors = require("colors." .. O.colorscheme).colors
local bg = colors.bg
local fg = colors.fg
local lightbg = colors.lightbg
local fgfaded = colors.fgfaded

require("bufferline").setup {
  options = {
    mappings = false,
    buffer_close_icon = "",
    modified_icon = "●",
    close_icon = "",
    left_trunc_marker = "",
    right_trunc_marker = "",
    max_name_length = 18,
    max_prefix_length = 15, -- prefix used when a buffer is de-duplicated
    tab_size = 18,
    offsets = {
      { filetype = "NvimTree", text = "File Explorer", text_align = "center" },
    },
    show_buffer_icons = true, -- disable filetype icons for buffers
    show_buffer_close_icons = true,
    show_close_icon = false,
    show_tab_indicators = true,
    separator_style = "thin",
    always_show_bufferline = true,
  },
  highlights = {
    fill = {
      guibg = bg,
    },
    background = {
      guibg = bg,
    },
    buffer_visible = {
      guifg = fgfaded,
      guibg = lightbg,
    },
    buffer_selected = {
      guifg = fg,
      guibg = lightbg,
    },
    indicator_selected = {
      guifg = lightbg,
      guibg = lightbg,
    },
    separator = {
      guifg = bg,
      guibg = bg,
    },
    separator_visible = {
      guifg = lightbg,
      guibg = lightbg,
    },
    separator_selected = {
      guifg = lightbg,
      guibg = lightbg,
    },
    modified = {
      guibg = bg,
    },
    modified_visible = {
      guibg = lightbg,
    },
    modified_selected = {
      guibg = lightbg,
    },
    close_button = {
      guibg = bg,
    },
    close_button_visible = {
      guibg = lightbg,
    },
    close_button_selected = {
      guibg = lightbg,
    },
    duplicate_visible = {
      guibg = lightbg,
    },
    duplicate_selected = {
      guibg = lightbg,
    },
  },
}

local opt = { silent = true }
local map = vim.api.nvim_set_keymap
vim.g.mapleader = " "

-- MAPPINGS
map("n", "<S-t>", [[<Cmd>tabnew<CR>]], opt) -- new tab
map("n", "<S-x>", [[<Cmd>bdelete<CR>]], opt) -- close tab
map("n", "<TAB>", [[<Cmd>BufferLineCycleNext<CR>]], opt)
map("n", "<S-TAB>", [[<Cmd>BufferLineCyclePrev<CR>]], opt)
