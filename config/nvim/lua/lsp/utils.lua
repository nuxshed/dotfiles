local M = {}

function M.toggle_format_on_save()
  O.format_on_save = not O.format_on_save
end

function M.format_on_save()
  if O.format_on_save then
    vim.lsp.buf.formatting_sync()
  end
end

return M
