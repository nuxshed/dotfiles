local M = {}

M.setup = function()
  local null_ls = require "null-ls"
  local b = null_ls.builtins

  null_ls.setup {
    debounce = 150,
    sources = {
      b.formatting.black,
      b.formatting.isort,
      b.formatting.prettierd,
      b.diagnostics.shellcheck,
      b.formatting.stylua,
    },
    on_attach = function(client)
      if client.resolved_capabilities.document_formatting then
        utils.augroup("lsp_format", {
          {
            events = { "BufWritePre" },
            targets = { "<buffer>" },
            command = require("lsp.utils").format_on_save,
          },
        })
      end
    end,
  }
end

return M
