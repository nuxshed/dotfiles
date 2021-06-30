-- indentline
vim.g.indent_blankline_char = "│"
vim.g.indent_blankline_use_treesitter = true
vim.g.indent_blankline_show_trailing_blankline_indent = false
vim.g.indent_blankline_filetype_exclude = {
  "dashboard",
  "log",
  "fugitive",
  "packer",
  "vimwiki",
  "markdown",
  "json",
  "txt",
  "help",
  -- "NvimTree",
  "git",
  "TelescopePrompt",
  "", -- for all buffers without a file type
}
vim.g.indent_blankline_buftype_exclude = { "terminal", "nofile" }
vim.g.indent_blankline_filetype_exclude = {
  "help",
  "dashboard",
  "packer",
  "NvimTree",
  "Trouble",
}
vim.g.indent_blankline_show_current_context = true
vim.g.indent_blankline_context_patterns = {
  "class",
  "return",
  "function",
  "method",
  "^if",
  "^while",
  "jsx_element",
  "^for",
  "^object",
  "^table",
  "block",
  "arguments",
  "if_statement",
  "else_clause",
  "jsx_element",
  "jsx_self_closing_element",
  "try_statement",
  "catch_clause",
  "import_statement",
  "operation_type",
}
vim.wo.colorcolumn = "99999"
