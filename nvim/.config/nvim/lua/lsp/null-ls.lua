local nls = require "null-ls"

local M = {}

vim.env.PRETTIERD_DEFAULT_CONFIG = vim.fn.stdpath "config" .. "/.prettierrc"

local function get_stylua_config()
  if vim.fn.filereadable(vim.fn.getcwd() .. "/stylua.toml") == 1 then
    return vim.fn.getcwd() .. "/stylua.toml"
  elseif vim.fn.filereadable(vim.fn.getcwd() .. "/.stylua.toml") == 1 then
    return vim.fn.stdpath "config" .. "/.stylua.toml"
  else
    return vim.fn.stdpath "config" .. "/stylua.toml"
  end
end

function M.setup(on_attach)
  nls.setup {
    on_attach = on_attach,
    sources = {
      nls.builtins.formatting.prettierd,
      nls.builtins.formatting.stylua.with {
        args = {
          "--config-path",
          get_stylua_config(),
          "-",
        },
      },
      nls.builtins.formatting.eslint_d,
      nls.builtins.formatting.black,
      nls.builtins.formatting.isort,
      nls.builtins.diagnostics.shellcheck,
      nls.builtins.rustfmt,
      nls.builtins.diagnostics.selene.with {
        args = {
          "--display-style",
          "json",
          "-",
        },
      },
      nls.builtins.chktex,
      nls.builtins.code_actions.gitsigns,
      nls.builtins.diagnostics.markdownlint,
    },
  }
end

function M.has_formatter(ft)
  local config = require "null-ls.config"
  local formatters = config.generators "NULL_LS_FORMATTING"
  for _, f in ipairs(formatters) do
    if vim.tbl_contains(f.filetypes, ft) then
      return true
    end
  end
end

return M
