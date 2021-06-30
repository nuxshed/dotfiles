local ts_config = require("nvim-treesitter.configs")

ts_config.setup({
  ensure_installed = {
    "bash",
    "c",
    "comment",
    "cpp",
    "css",
    "dart",
    "html",
    "javascript",
    "json",
    "lua",
    "python",
    "rust",
    "scss",
    "toml",
    "typescript",
    "yaml",
  },
  highlight = {
    enable = true,
    use_languagetree = true,
  },
})
