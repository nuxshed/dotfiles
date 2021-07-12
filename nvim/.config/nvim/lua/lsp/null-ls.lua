local nls = require("null-ls")

local M = {}

function M.setup(on_attach)
  nls.setup({
    on_attach = on_attach,
    sources = {
      nls.builtins.formatting.prettierd,
      nls.builtins.formatting.stylua,
      nls.builtins.diagnostics.selene,
      nls.builtins.formatting.eslint_d,
      nls.builtins.diagnostics.shellcheck,
      nls.builtins.formatting.black,
      nls.builtins.formatting.isort,
      nls.builtins.code_actions.gitsigns,
    },
  })
end

function M.has_formatter(ft)
  local config = require("null-ls.config")
  local formatters = config.generators("NULL_LS_FORMATTING")
  for _, f in ipairs(formatters) do
    if vim.tbl_contains(f.filetypes, ft) then
      return true
    end
  end
end

return M
