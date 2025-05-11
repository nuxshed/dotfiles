-- lsp/init.lua

local lspconfig = require "lspconfig"
local mason_lspconfig = require "mason-lspconfig"

mason_lspconfig.setup_handlers {
  function(server_name)
    lspconfig[server_name].setup {}
  end,
}

lspconfig.lua_ls.setup {
  settings = {
    Lua = {
      runtime = {
        version = "LuaJIT",
        path = vim.split(package.path, ";"),
      },
      diagnostics = {
        globals = { "vim" },
      },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true),
        checkThirdParty = false,
      },
      telemetry = {
        enable = false,
      },
    },
  },
}

lspconfig.clangd.setup {
  -- 	cmd = { "clangd", "--background-index", "--clang-tidy", "--log=verbose" },
  -- 	init_options = {
  -- 		fallbackFlags = { "-std=c++17" },
  -- 	},
}

vim.diagnostic.config {
  signs = true,
  underline = true,
  update_in_insert = false,
  virtual_text = {
    spacing = 4,
    prefix = "",
  },
  severity_sort = true,
}