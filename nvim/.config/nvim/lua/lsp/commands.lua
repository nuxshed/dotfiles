local utils = require "utils"

local M = {}

function M.setup()
  utils.lua_command("LspDef", "vim.lsp.buf.definition()")
  utils.lua_command("LspTypeDef", "vim.lsp.buf.type_definition()")
  utils.lua_command("LspImplementation", "vim.lsp.buf.implementation()")
  utils.lua_command("LspReferences", "vim.lsp.buf.references()")
  utils.lua_command("LspRename", "vim.lsp.buf.rename()")
  utils.lua_command("LspHover", "vim.lsp.buf.hover()")
  utils.lua_command("LspFormat", "vim.lsp.buf.formatting()")
  utils.command("LspLog", "edit " .. vim.lsp.get_log_path())
end

return M
