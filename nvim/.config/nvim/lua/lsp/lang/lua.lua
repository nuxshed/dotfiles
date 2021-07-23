local M = {}

M.setup = function(on_attach)
  -- require("lspconfig").lua.setup({
  --   require("lua-dev").setup({}),
  -- })
  require("lspconfig")["lua"].setup {
    root_dir = vim.loop.cwd,
    on_attach = on_attach,
    settings = {
      Lua = {
        diagnostics = {
          globals = { "vim" },
        },
        workspace = {
          library = {
            [vim.fn.expand "$VIMRUNTIME/lua"] = true,
            [vim.fn.expand "$VIMRUNTIME/lua/vim/lsp"] = true,
          },
          maxPreload = 100000,
          preloadFileSize = 10000,
        },
        telemetry = {
          enable = false,
        },
      },
    },
  }
end

return M
