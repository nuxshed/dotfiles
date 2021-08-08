local ts_config = require "nvim-treesitter.configs"

ts_config.setup {
  ensure_installed = O.treesitter.ensure_installed,
  highlight = {
    enable = O.treesitter.highlight,
    use_languagetree = true,
  },
  indent = { enable = false },
  context_commentstring = { enable = true },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
}

-- Add Markdown
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.jsonc.used_by = "json"
parser_config.markdown = {
  install_info = {
    url = "https://github.com/ikatyang/tree-sitter-markdown",
    files = { "src/parser.c", "src/scanner.cc" },
  },
}
