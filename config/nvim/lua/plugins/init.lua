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
  -- zenbones.nvim
  {
    "zenbones-theme/zenbones.nvim",
    dependencies = "rktjmp/lush.nvim",
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
  -- telescope.nvim
  {
    "nvim-telescope/telescope.nvim",
    requires = { "nvim-lua/plenary.nvim" },
    dependencies = {
      { "nvim-telescope/telescope-file-browser.nvim" },
      { "nvim-telescope/telescope-live-grep-raw.nvim" },
    },
    config = function()
      require "plugins.telescope"
    end,
  },
  -- icon provider
  { "nvim-tree/nvim-web-devicons" },
  -- which-key
  {
    "folke/which-key.nvim",
    config = function()
      require "plugins.which-key"
    end,
  },
  -- treesj
  {
    "Wansmer/treesj",
    -- keys = { "<space>m", "<space>j", "<space>s" },
    config = function()
      require("treesj").setup()
    end,
  },
  -- alpha.nvim
  {
    "goolord/alpha-nvim",
    requires = { "kyazdani42/nvim-web-devicons" },
    config = function()
      require "plugins.alpha"
    end,
  },
  -- auto pairs
  {
    "windwp/nvim-autopairs",
    config = function()
      require("nvim-autopairs").setup{
        close_triple_quotes = true,
      }
    end,
  },
  -- git signs
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require "plugins.gitsigns"
    end,
  },
  -- comment.nvim
  {
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup()
    end,
  },
  -- trouble.nvim
  {
    "folke/trouble.nvim",
    config = function()
      require("trouble").setup()
    end,
  },
  -- luasnip
  {
    "L3MON4D3/LuaSnip",
    dependencies = { "rafamadriz/friendly-snippets" },
    config = function()
      require("luasnip").config.set_config {
        history = true,
        updateevents = "TextChanged,TextChangedI",
      }

      require("luasnip/loaders/from_vscode").lazy_load()
    end,
  },
  -- nvim-tree
  {
    "kyazdani42/nvim-tree.lua",
    config = function()
      require "plugins.nvim-tree"
    end,
  },

  -- zen-mode and twilight
  {
    "folke/zen-mode.nvim",
    cmd = { "ZenMode" },
    config = function()
      require("zen-mode").setup {
        window = { backdrop = 1, width = 80 },
        plugins = {
          twilight = { enabled = false },
          gitsigns = { enabled = true },
          kitty = { enabled = false, increment = "+1" },
        },
      }
    end,
  },
  {
    "folke/twilight.nvim",
    cmd = "Twilight",
  },

  -- range-highlight
  {
    "winston0410/range-highlight.nvim",
    dependencies = { "winston0410/cmd-parser.nvim" },
    config = function()
      require("range-highlight").setup()
    end,
  },

  -- nvim-colorizer.lua
  {
    "norcalli/nvim-colorizer.lua",
    config = function()
      require("colorizer").setup()
    end,
  },

  -- precognition.nvim
  {
    "tris203/precognition.nvim",
    event = "VeryLazy",
    opts = {
      startVisible = false,
    }
  },

  -- leetcode.nvim
  {
    "kawre/leetcode.nvim",
    dependencies = {
        "nvim-telescope/telescope.nvim",
        "nvim-lua/plenary.nvim",
        "MunifTanjim/nui.nvim",
    },
    opts = {
      lang = "c",
    }
  },

  -- auto-formatting
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        lua = { "stylua" },
        c = { "clang-format" },
        rust = { "rustfmt" },
        python = { "black" },
        bash = { "shfmt" },
      }
    }
  }
}
