-- config file for my neovim configuration

-- inspired by LunarVim's configuration
-- https://github.com/ChristianChiarulli/LunarVim

O = {
  colorscheme = "tokyonight",
  leader = " ",
  format_on_save = true,
  more_status = false, -- show more stuff in statusline (buftype, encoding, etc.)
  smart_close = "help,startuptime,qf,lspinfo", -- windows to close with 'q'
  statusline = {
    CurrentDir = true,
    GitRoot = false,
    BufType = true,
    Encoding = true,
  },
  treesitter = {
    ensure_installed = {
      "bash",
      "c",
      "comment",
      "cpp",
      "css",
      "dart",
      "go",
      "html",
      "javascript",
      "json",
      "lua",
      "markdown",
      "python",
      "rust",
      "scss",
      "toml",
      "typescript",
      "tsx",
      "vue",
      "yaml",
    },
    highlight = { enabled = true },
    incremental_selection = { enabled = true },
  },
  plugin = {
    dashboard = { enabled = true },
    colorizer = { enabled = true },
    neogit = { enabled = true },
    octo = { enabled = false },
    diffview = { enabled = false },
    lspsaga = { enabled = false },
    trouble = { enabled = true },
    codi = { enabled = false },
  },
  lang = {
    angular = {},
    bash = {},
    cpp = {},
    css = {},
    deno = {},
    html = {
      init_options = {
        configurationSection = { "html", "css", "javascript" },
        embeddedLanguages = {
          css = true,
          javascript = true,
        },
      },
    },
    json = {},
    lua = {
      luadev = {
        -- don't load luadev unless explicitly asked to
        enabled = false,
      },
    },
    python = {
      analysis = {
        type_checking = "basic",
        auto_search_paths = true,
        use_library_code_types = true,
      },
    },
    rust = {
      rust_tools = {
        enabled = true,
      },
      settings = {
        ["rust-analyzer"] = {
          -- rust-analyzer settings here
        },
      },
      -- this depends on lspconfig, which is a plugin
      -- may cause issues with bootstrapping
      -- root_dir = require("lspconfig.util").root_pattern("Cargo.toml", "rust-project.json", ".git"),
    },
    typescript = {
      filetypes = { "javascript", "typescript", "typescriptreact" },
    },
    vim = {},
    vue = {},
    yaml = {},
  },
  terminal = {
    direction = "horizontal",
  },
}

-- source project config
if vim.fn.filereadable(vim.fn.getcwd() .. "/.nvim-config.lua") == 1 then
  vim.cmd("source" .. vim.fn.getcwd() .. "/.nvim-config.lua")
end
