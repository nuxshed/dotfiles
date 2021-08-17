local utils = require "utils"

local M = {}

-- TODO: implement function to toggle format on save

function M.setup(client, bufnr)
  -- local ft = vim.api.nvim_buf_get_option(bufnr, "filetype")
  -- local nls = require "lsp.null-ls"

  -- local enable = false
  -- if nls.has_formatter(ft) then
  --   enable = client.name == "null-ls"
  -- else
  --   -- enable = not (client.name == "null-ls")
  --   enable = true
  -- end

  client.resolved_capabilities.document_formatting = client.name == "null-ls"
end

return M
