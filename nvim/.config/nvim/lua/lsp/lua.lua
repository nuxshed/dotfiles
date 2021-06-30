local M = {}

M.setup = function(on_attach)
  local luadev = require("lua-dev").setup({
    lspconfig = {
      runtime = {
        version = "LuaJIT",
      },
      diagnostics = {
        globals = { "vim" },
      },
      workspace = {
        library = {
          [vim.fn.expand("$VIMRUNTIME/lua")] = true,
          [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
        },
      },
      on_attach = on_attach
    },
  })

  require("lspconfig").lua.setup(luadev)
end

return M
