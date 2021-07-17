-- config file for my neovim configuration
-- lol

-- inspired by LunarVim's configuration
-- https://github.com/ChristianChiarulli/LunarVim
-- WIP

O = {
  colorscheme = "onedark",
  leader = " ",
  format_on_save = true,
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
    octo = { enabled = false },
    diffview = { enabled = false },
    lspsaga = { enabled = true },
    trouble = { enabled = true },
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
        enabled = true,
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
      root_dir = require("lspconfig.util").root_pattern("Cargo.toml", "rust-project.json", ".git"),
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
