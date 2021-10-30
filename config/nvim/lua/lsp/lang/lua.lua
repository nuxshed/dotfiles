local M = {}

M.setup = {
  cmd = { "lua-language-server" },
  root_dir = require("lspconfig.util").root_pattern("stylua.toml", "rc.lua", ".git") or vim.loop.cwd,
  settings = {
    Lua = {
      completion = {
        enable = true,
        callSnippet = "Replace",
      },
      runtime = {
        version = "LuaJIT",
        path = (function()
          local runtime_path = vim.split(package.path, ";")
          table.insert(runtime_path, "lua/?.lua")
          table.insert(runtime_path, "lua/?/init.lua")
          return runtime_path
        end)(),
      },
      diagnostics = {
        enable = true,
        globals = {
          "vim",
          "O",
          "utils",
        },
      },
      telemetry = {
        enable = false,
      },
    },
  },
}

return M
