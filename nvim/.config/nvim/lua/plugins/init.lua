local fn = vim.fn
local exec = vim.api.nvim_command

vim.cmd [[ packadd packer.nvim ]]

-- Bootstrapping packer -----------------------------------------------------------------
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
  vim.notify "Downloading packer.nvim..."
  exec("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
  exec "packadd packer.nvim"
end
-----------------------------------------------------------------------------------------

local config = {
  profile = {
    enable = true,
    threshold = 1, -- the amount in ms that a plugins load time must be over for it to be included in the profile
  },
}

require("packer").startup {
  function(use)
    -- Core
    use { "wbthomason/packer.nvim" }

    use {
      "akinsho/nvim-bufferline.lua",
      event = "BufWinEnter",
      config = [[require("plugins.bufferline")]],
      requires = "nvim-web-devicons",
    }

    use {
      "glepnir/galaxyline.nvim",
      event = "BufEnter",
      config = [[require("plugins.statusline")]],
    }

    use {
      "nvim-telescope/telescope.nvim",
      requires = {
        "nvim-lua/popup.nvim",
      },
      config = [[require("plugins.telescope")]],
      event = "CursorHold",
    }

    use {
      "akinsho/nvim-toggleterm.lua",
      keys = { "<M-`>", "<leader>g" },
      config = function()
        require("toggleterm").setup {
          size = function(term)
            if term.direction == "horizontal" then
              return 20
            elseif term.direction == "vertical" then
              return vim.o.columns
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

        local Terminal = require("toggleterm.terminal").Terminal
        local lazygit = Terminal:new {
          cmd = "lazygit",
          dir = "git_dir",
          direction = "float",
          float_opts = {
            border = "single",
          },
          on_open = function(term)
            vim.cmd "startinsert!"
            vim.api.nvim_buf_set_keymap(
              term.bufnr,
              "n",
              "q",
              "<cmd>close<CR>",
              { noremap = true, silent = true }
            )
          end,
        }

        function _lazygit_toggle()
          lazygit:toggle()
        end

        -- vim.api.nvim_set_keymap("n", "<leader>g", "<cmd>lua _lazygit_toggle()<CR>", { noremap = true, silent = true })
        require("utils").map(
          "n",
          "<leader>g",
          [[ <Cmd> lua _lazygit_toggle()<CR>]]
        )
      end,
    }

    use "nvim-lua/plenary.nvim"
    use {
      "kyazdani42/nvim-web-devicons",
      module = "nvim-web-devicons",
      config = [[require("plugins.devicons")]],
    }

    use {
      "lukas-reineke/indent-blankline.nvim",
      setup = [[require("plugins.indentline")]],
      event = "BufRead",
    }

    use {
      "kyazdani42/nvim-tree.lua",
      requires = "nvim-web-devicons",
      config = [[require("plugins.nvim-tree")]],
      cmd = "NvimTreeToggle",
      keys = { "<C-n>" },
    }

    use {
      "folke/trouble.nvim",
      requires = "nvim-web-devicons",
      cmd = { "Trouble", "TroubleClose", "TroubleToggle", "TodoTrouble" },
      condition = O.plugin.trouble.enabled,
    }

    use {
      "glepnir/dashboard-nvim",
      config = [[require("plugins.dashboard")]],
      condition = O.plugin.dashboard.enabled,
    }

    -- LSP, Debugging, Completion and Snippets
    use {
      "neovim/nvim-lspconfig",
      config = [[require("lsp")]],
      requires = {
        {
          "nvim-lua/lsp-status.nvim",
          config = function()
            local status = require "lsp-status"
            status.config {
              indicator_hint = "",
              indicator_info = "",
              indicator_errors = "✗",
              indicator_warnings = "",
              status_symbol = " ",
            }
            status.register_progress()
          end,
        },
        {
          "glepnir/lspsaga.nvim",
          config = [[require("plugins.lspsaga")]],
          condition = O.plugin.lspsaga.enabled,
          after = "nvim-lspconfig",
        },
        { "kabouzeid/nvim-lspinstall" },
      },
    }

    -- use({
    --   "simrat39/rust-tools.nvim",
    --   condition = O.lang.rust.rust_tools.enabled,
    --   config = function()
    --     local on_attach = require("lsp").on_attach
    --     require("lsp.lang.rust").setup(on_attach)
    --   end,
    --   ft = "rust",
    --   event = "BufRead",
    -- })
    use { "jose-elias-alvarez/null-ls.nvim" }

    use { "folke/lua-dev.nvim" }

    use {
      "hrsh7th/nvim-compe",
      config = [[require("plugins.compe")]],
      event = "InsertEnter",
      wants = { "LuaSnip" },
    }

    use {
      "L3MON4D3/LuaSnip",
      event = "InsertEnter",
      requires = {
        { "rafamadriz/friendly-snippets", event = "InsertCharPre" },
        "hrsh7th/nvim-compe",
      },
      config = function()
        require("luasnip").config.set_config {
          history = true,
          updateevents = "TextChanged,TextChangedI",
        }
        require("luasnip/loaders/from_vscode").load()
      end,
    }

    use {
      "onsails/lspkind-nvim",
      config = [[require("lspkind").init()]],
      after = "nvim-compe",
    }

    use { "jose-elias-alvarez/nvim-lsp-ts-utils" }

    -- syntax
    use {
      "nvim-treesitter/nvim-treesitter",
      run = ":TSUpdate",
      config = [[require("plugins.treesitter")]],
      -- event = "BufRead",
    }

    -- comment
    use {
      "terrortylor/nvim-comment",
      config = function()
        require("nvim_comment").setup()
      end,
      keys = "gc",
    }

    use {
      "folke/todo-comments.nvim",
      config = [[require("todo-comments").setup()]],
      event = "BufEnter",
    }

    -- git
    use {
      "lewis6991/gitsigns.nvim",
      config = [[require("plugins.gitsigns")]],
    }

    use {
      "TimUntersberger/neogit",
      condition = O.plugin.neogit.enabled,
      cmd = { "Neogit" },
      config = [[require("plugins.neogit")]],
    }

    use {
      "pwntester/octo.nvim",
      cmd = { "Octo" },
      condition = O.plugin.octo.enabled,
    }

    use {
      "sindrets/diffview.nvim",
      cmd = {
        "DiffviewOpen",
        "DiffviewClose",
        "DiffviewToggleFiles",
        "DiffviewFocusFiles",
        "DiffviewRefresh",
      },
      condition = O.plugin.diffview.enabled,
    }

    -- interactive scratchpad
    use {
      "metakirby5/codi.vim",
      cmd = "Codi",
      condition = O.plugin.codi.enabled,
    }

    -- tests
    use {
      "vim-test/vim-test",
      cmd = { "TestFile", "TestNearest", "TestSuite", "TestVisit" },
      setup = function()
        local map = require("utils").map
        map("n", "<leader>tn", [[ <Cmd> TestNearest<CR>]])
        map("n", "<leader>tf", [[ <Cmd> TestFile<CR>]])
        map("n", "<leader>ts", [[ <Cmd> TestSuite<CR>]])
        map("n", "<leader>tl", [[ <Cmd> TestLast<CR>]])
        map("n", "<leader>tv", [[ <Cmd> TestVisit<CR>]])
        vim.g["test#strategy"] = "neovim"
      end,
    }

    -- plugin for live html, css, and javascript editing
    -- use({
    --   "turbio/bracey.vim",
    --   event = "BufRead",
    --   ft = { "html", "css", "js" },
    --   run = "npm install --prefix server",
    -- })

    -- Utilities
    -- use({ "milisims/nvim-luaref" })
    use {
      "windwp/nvim-autopairs",
      config = [[require("nvim-autopairs").setup()]],
      after = "nvim-compe",
    }
    use {
      "karb94/neoscroll.nvim",
      config = [[require("neoscroll").setup()]],
      event = "WinScrolled",
    }
    use { "folke/which-key.nvim", config = [[require("which-key").setup()]] }
    use {
      "simrat39/symbols-outline.nvim",
      cmd = { "SymbolsOutline", "SymbolsOutlineOpen", "SymbolsOutlineClose" },
    }
    -- use({ "sudormrfbin/cheatsheet.nvim" })

    -- markdown
    use {
      "plasticboy/vim-markdown",
      opt = true,
      requires = "godlygeek/tabular",
      ft = "markdown",
    }

    -- latex
    use {
      "lervag/vimtex",
      ft = "latex",
      setup = function()
        vim.g.vimtex_quickfix_enabled = false
        vim.g.vimtex_view_method = "zathura"
        vim.g.vimtex_compiler_latexmk = {
          options = {
            "--shell-escape",
            "--verbose",
            "--file-line-error",
            "synctex=1",
            "interaction=nonstopmode",
          },
        }
      end,
    }
    use {
      "folke/zen-mode.nvim",
      cmd = "ZenMode",
    }
    -- use({ "folke/twilight.nvim" })
    -- use("phaazon/hop.nvim")
    use { "tweekmonster/startuptime.vim", cmd = "StartupTime" }

    -- colors
    use { "siduck76/nvim-base16.lua" }
    use {
      "norcalli/nvim-colorizer.lua",
      config = [[require'colorizer'.setup()]],
      condition = O.plugin.colorizer.enabled,
      event = "BufEnter",
    }
  end,
  config = config,
}
