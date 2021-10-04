local augroup = utils.augroup

local M = {}

function M.setup(client)
  -- format-on-save
  if client and client.resolved_capabilities.document_formatting then
    augroup("lsp_format", {
      {
        events = { "BufWritePre" },
        targets = { "<buffer>" },
        command = require("lsp.utils").format_on_save,
      },
    })
  end
  -- cursor commands
  if client and client.resolved_capabilities.document_highlight then
    augroup("lsp_document_highlight", {
      {
        events = { "CursorHold" },
        targets = { "<buffer>" },
        command = vim.lsp.buf.document_highlight,
      },
      {
        events = { "CursorHoldI" },
        targets = { "<buffer>" },
        command = "silent! lua vim.lsp.buf.document_highlight()",
      },
      {
        events = { "CursorMoved" },
        targets = { "<buffer>" },
        command = vim.lsp.buf.clear_references,
      },
    })
  end
end

return M
