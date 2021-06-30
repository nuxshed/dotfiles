local fn = vim.fn
local exec = vim.api.nvim_command

-- Bootstrapping packer -----------------------------------------------------------------
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
  vim.notify("Downloading packer.nvim...")
  exec("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
  exec("packadd packer.nvim")
end
-----------------------------------------------------------------------------------------

local config = {
  profile = {
    enable = true,
    threshold = 1, -- the amount in ms that a plugins load time must be over for it to be included in the profile
  },
}

require("packer").startup({
  function(use)
    -- Core
    use("wbthomason/packer.nvim")

    use({
      "akinsho/nvim-bufferline.lua",
      config = [[require("plugins.bufferline")]],
    })

    use({
      "glepnir/galaxyline.nvim",
      config = [[require("plugins.statusline")]],
    })

    use({
      "nvim-telescope/telescope.nvim",
      requires = {
        "nvim-lua/popup.nvim",
      },
      config = [[require("plugins.telescope")]],
    })

    use({
      "akinsho/nvim-toggleterm.lua",
      config = [[require("plugins.terminal")]],
    })

    use("nvim-lua/plenary.nvim")
    use({ "kyazdani42/nvim-web-devicons", config = [[require("plugins.devicons")]] })
    use({
      "lukas-reineke/indent-blankline.nvim",
      branch = "lua",
      config = [[require("plugins.indentline")]],
    })

    use({
      "kyazdani42/nvim-tree.lua",
      requires = "nvim-web-devicons",
      config = [[require("plugins.nvim-tree")]],
    })

    use({
      "folke/trouble.nvim",
      requires = "nvim-web-devicons",
    })

    use({ "glepnir/dashboard-nvim", config = [[require("plugins.dashboard")]] })

    -- LSP, Debugging, Completion and Snippets
    use({
      "neovim/nvim-lspconfig",
      config = [[require("lsp.config")]],
      requires = {
        {
          "nvim-lua/lsp-status.nvim",
          config = function()
            local status = require("lsp-status")
            status.config({
              indicator_hint = "",
              indicator_info = "",
              indicator_errors = "✗",
              indicator_warnings = "",
              status_symbol = " ",
            })
            status.register_progress()
          end,
        },
        { "onsails/lspkind-nvim", config = [[require("lspkind").init()]] },
        { "glepnir/lspsaga.nvim" },
        { "kabouzeid/nvim-lspinstall" },
      },
    })

    use({ "jose-elias-alvarez/null-ls.nvim" })

    -- use { "mfussenegger/nvim-dap" }
    -- use {
    --   "rcarriga/nvim-dap-ui",
    --   requires = "nvim-dap",
    --   after = "nvim-dap",
    --   config = [[require("dapui").setup()]]
    -- }

    use({ "folke/lua-dev.nvim" })

    use({
      "hrsh7th/nvim-compe",
      config = [[require("plugins.compe")]],
      event = "InsertEnter",
    })

    -- use {
    -- "hrsh7th/vim-vsnip",
    -- requires = { "rafamadriz/friendly-snippets", "hrsh7th/nvim-compe" },
    -- }

    use({
      "L3MON4D3/LuaSnip",
      requires = { "rafamadriz/friendly-snippets", "hrsh7th/nvim-compe" },
      config = function()
        require("luasnip").config.set_config({
          history = true,
          updateevents = "TextChanged,TextChangedI",
        })
        require("luasnip/loaders/from_vscode").load()
      end,
    })

    use({ "jose-elias-alvarez/nvim-lsp-ts-utils", ft = "typescript" })

    -- syntax
    use({
      "nvim-treesitter/nvim-treesitter",
      run = ":TSUpdate",
      config = [[require("plugins.treesitter")]],
    })

    use("cespare/vim-toml")

    -- comment
    use({
      "terrortylor/nvim-comment",
      config = function()
        require("nvim_comment").setup()
      end,
    })

    use({
      "folke/todo-comments.nvim",
      config = [[require("plugins.todo-comments")]],
    })

    -- git
    use("lewis6991/gitsigns.nvim")

    -- Utilities
    use("milisims/nvim-luaref")
    use({
      "windwp/nvim-autopairs",
      config = [[require("nvim-autopairs").setup()]],
      event = "BufRead",
    })
    use("karb94/neoscroll.nvim")
    use("folke/which-key.nvim")
    use({ "simrat39/symbols-outline.nvim", cmd = { "SymbolsOutline", "SymbolsOutlineOpen", "SymbolsOutlineClose" } })

    use({
      "plasticboy/vim-markdown",
      opt = true,
      requires = "godlygeek/tabular",
      ft = "markdown",
    })
    use({
      "iamcco/markdown-preview.nvim",
      ft = "markdown",
      cmd = "MarkdownPreview",
    })

    use("folke/zen-mode.nvim")
    use("phaazon/hop.nvim")
    use("tweekmonster/startuptime.vim")

    -- colors
    use("siduck76/nvim-base16.lua")
    use({ "norcalli/nvim-colorizer.lua", config = [[require'colorizer'.setup()]] })
  end,
  config = config,
})
