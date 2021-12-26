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
  }
end

return M
