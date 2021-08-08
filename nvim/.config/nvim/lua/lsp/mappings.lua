local utils = require "utils"

local M = {}

function M.setup(bufnr)
  local wk = require "which-key"
  utils.buf_map("n", "K", ":Lspsaga hover_doc<CR>", nil, bufnr)
  utils.buf_map("n", "[d", ":Lspsaga diagnostic_jump_prev<CR>", nil, bufnr)
  utils.buf_map("n", "]d", ":Lspsaga diagnostic_jump_next<CR>", nil, bufnr)

  wk.register({
    name = "+code",
    r = { "<CMD>LspRename<CR>", "rename" },
    a = { "<CMD>Lspsaga code_action<CR>", "code action" },
    d = { "<CMD>Lspsaga show_line_diagnostics<CR>", "line diagnostics" },
    l = {
      name = "+lsp",
      i = { "<cmd>LspInfo<cr>", "Lsp Info" },
      a = { "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", "Add Folder" },
      r = {
        "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>",
        "Remove Folder",
      },
      l = {
        "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>",
        "List Folders",
      },
    },
  }, {
    buffer = bufnr,
    prefix = "c",
  })

  wk.register({
    name = "+goto",
    d = { "<CMD>LspDef<CR>", "definition" },
    D = { "<CMD>LspTypeDef<CR>", "type definition" },
    i = { "<CMD>LspImplementation<CR>", "implementation" },
    R = { "<CMD>LspReferences<CR>", "references" },
  }, {
    buffer = bufnr,
    prefix = "g",
  })
end

return M
