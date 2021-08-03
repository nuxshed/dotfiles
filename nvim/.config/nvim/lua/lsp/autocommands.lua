local utils = require "utils"
local augroup = utils.augroup

local M = {}

function M.setup(client)
  -- format-on-save
  if O.format_on_save and client.resolved_capabilities.document_formatting then
    vim.cmd "autocmd BufWritePost * :LspFormat"
  end
  -- cursor commands
  if client and client.resolved_capabilities.document_highlight then
    -- vim.api.nvim_exec(
    --   [[augroup lsp_document_highlight
    --       autocmd! * <buffer>
    --       autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
    --       autocmd CursorHoldI <buffer> silent! lua vim.lsp.document_highlight()
    --       autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
    --     augroup END]],
    --   false
    -- )
    augroup("lsp_document_highlight", {
      {
        events = { "CursorHold" },
        targets = { "<buffer>" },
        command = "lua vim.lsp.buf.document_highlight()",
      },
      {
        events = { "CursorHoldI" },
        targets = { "<buffer>" },
        command = "silent! lua vim.lsp.buf.document_highlight()",
      },
      {
        events = { "CursorMoved" },
        targets = { "<buffer>" },
        command = "lua vim.lsp.buf.clear_references()",
      },
    })
  end
end

return M
