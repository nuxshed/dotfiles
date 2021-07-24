local M = {}

function M.setup(on_attach)
  local luadev = require("lua-dev").setup {
    library = {
      vimruntime = true,
      types = true,
      plugins = true,
      -- plugins = { "nvim-treesitter", "plenary.nvim"},
    },
    lspconfig = {
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
    },
  }

  require("lspconfig").lua.setup(luadev)
  -- require("lspconfig")["lua"].setup {
  --   root_dir = vim.loop.cwd,
  --   on_attach = on_attach,
  --   settings = {
  --     Lua = {
  --       diagnostics = {
  --         globals = { "vim" },
  --       },
  --       workspace = {
  --         library = {
  --           [vim.fn.expand "$VIMRUNTIME/lua"] = true,
  --           [vim.fn.expand "$VIMRUNTIME/lua/vim/lsp"] = true,
  --         },
  --         maxPreload = 100000,
  --         preloadFileSize = 10000,
  --       },
  --       telemetry = {
  --         enable = false,
  --       },
  --     },
  --   },
  -- }
end

return M
