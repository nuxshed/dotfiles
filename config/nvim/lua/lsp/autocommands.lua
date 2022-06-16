local augroup = utils.augroup

local M = {}

function M.setup(client)
  -- cursor commands
  if client and client.server_capabilities.document_highlight then
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
