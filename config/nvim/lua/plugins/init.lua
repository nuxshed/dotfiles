-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup {
  -- monochrome.nvim
  {
    "kdheepak/monochrome.nvim",
  },
  -- no-clown-fiesta.nvim
  {
    "aktersnurra/no-clown-fiesta.nvim",
  },
  -- nvim-lspconfig
  {
    "neovim/nvim-lspconfig",
    config = function()
      require "lsp"
    end,
  },
  -- mason.nvim
  {
    "williamboman/mason.nvim",
    config = function()
      require("mason").setup()
    end,
  },
  -- mason-lspconfig.nvim
  {
    "williamboman/mason-lspconfig.nvim",
    config = function()
      require("mason-lspconfig").setup {
        ensure_installed = {
          "lua_ls",
          "rust_analyzer",
          "clojure_lsp",
        },
      }
    end,
  },
  {
    "saghen/blink.cmp",
    version = "v0.11.0",
    dependencies = {
      "mikavilpas/blink-ripgrep.nvim",
    },
    opts = {
      keymap = { preset = "super-tab" },
      appearance = {
        nerd_font_variant = "mono",
      },
      sources = {
        default = { "lsp", "path", "snippets", "buffer", "ripgrep" },
        providers = {
          ripgrep = {
            module = "blink-ripgrep",
            name = "ripgrep",
          },
        },
      },
      fuzzy = {
        prebuilt_binaries = {
          download = true,
          force_version = "v0.11.0",
        },
      },
    },
    opts_extend = { "sources.default" },
  },
  -- nvim-treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
    config = function()
      require "plugins.treesitter"
    end,
  },
}
