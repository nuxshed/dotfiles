local lspconfig = require "lspconfig"
local ts_utils = require "nvim-lsp-ts-utils"

local M = {}

local ts_utils_settings = {
  -- debug = true,
  enable_import_on_completion = true,
  import_all_scan_buffers = 100,
  complete_parens = true,
  signature_help_in_parens = true,

  -- eslint
  eslint_bin = "eslint_d",
  eslint_enable_diagnostics = true,
  eslint_enable_code_actions = true,

  -- formatting
  enable_formatting = true,
  formatter = "eslint_d",
  eslint_config_fallback = vim.fn.stdpath "config" .. "/.eslintrc.js",

  -- update imports on file move
  update_imports_on_move = true,
}

M.setup = function(on_attach)
  lspconfig.typescript.setup {
    on_attach = function(client, bufnr)
      on_attach(client)

      ts_utils.setup(ts_utils_settings)
      ts_utils.setup_client(client)
    end,
    filetypes = O.lang.typescript.filetypes,
  }
end
return M
