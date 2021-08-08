local M = {}

if O.lang.lua.luadev.enabled then
  function M.setup(on_attach)
    local luadev = require("lua-dev").setup {
      library = {
        vimruntime = true,
        types = true,
        plugins = true,
        -- plugins = { "nvim-treesitter", "plenary.nvim"},
      },
      lspconfig = {
        root_dir = vim.loop.cwd,
        on_attach = on_attach,
        settings = {
          Lua = {
            diagnostics = {
              globals = { "vim", "O" },
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
  end
else
  function M.setup(on_attach)
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
end

return M
