local ts_config = require "nvim-treesitter.configs"

ts_config.setup {
  ensure_installed = O.treesitter.ensure_installed,
  highlight = {
    enable = O.treesitter.highlight,
    use_languagetree = true,
  },
}
