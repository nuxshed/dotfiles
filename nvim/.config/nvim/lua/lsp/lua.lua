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
      on_attach = on_attach,
    },
  })

  if O.lang.lua.luadev.enabled then
    require("lspconfig").lua.setup(luadev)
  else
    require("lspconfig").lua.setup({})
  end
end

return M
