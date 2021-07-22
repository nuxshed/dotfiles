local utils = require("utils")

local M = {}

M.setup = function(bufnr)
  utils.buf_map("n", "gd", ":LspDef<CR>", nil, bufnr)
  utils.buf_map("n", "gD", ":LspTypeDef<CR>", nil, bufnr)
  utils.buf_map("n", "gi", ":LspImplementation<CR>", nil, bufnr)
  utils.buf_map("n", "gr", ":LspReferences<CR>", nil, bufnr)
  utils.buf_map("n", "rn", ":LspRename<CR>", nil, bufnr)
  utils.buf_map("n", "K", ":LspHover<CR>", nil, bufnr)
  utils.buf_map("n", "gl", ":Lspsaga show_line_diagnostics<CR>", nil, bufnr)
  utils.buf_map("n", "ca", ":Lspsaga code_action<CR>", nil, bufnr)
  utils.buf_map("n", "[d", ":Lspsaga diagnostic_jump_prev<CR>", nil, bufnr)
  utils.buf_map("n", "]d", ":Lspsaga diagnostic_jump_next<CR>", nil, bufnr)
end

return M
