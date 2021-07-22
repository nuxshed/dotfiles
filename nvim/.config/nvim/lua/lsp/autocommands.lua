local utils = require("utils")

local M = {}

M.setup = function(client)
  -- format-on-save
  if client.resolved_capabilities.document_formatting and vim.g.autoformat then
    utils.augroup("LspFormatOnSave", "BufWritePost", "LspFormat")
  end
  -- cursor commands
  if client and client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec(
      [[augroup lsp_document_highlight
          autocmd! * <buffer>
          autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
          autocmd CursorHoldI <buffer> silent! lua vim.lsp.document_highlight()
          autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
        augroup END]],
      false
    )
  end
end

return M
