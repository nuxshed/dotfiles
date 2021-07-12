local utils = require("utils")

-- replace the default lsp diagnostic letters with prettier symbols
vim.fn.sign_define("LspDiagnosticsSignError", { text = "", numhl = "LspDiagnosticsDefaultError" })
vim.fn.sign_define("LspDiagnosticsSignWarning", { text = "", numhl = "LspDiagnosticsDefaultWarning" })
vim.fn.sign_define("LspDiagnosticsSignInformation", { text = "", numhl = "LspDiagnosticsDefaultInformation" })
vim.fn.sign_define("LspDiagnosticsSignHint", { text = "", numhl = "LspDiagnosticsDefaultHint" })

require("vim.lsp.protocol").CompletionItemKind = {
  " Text", -- Text
  " Method", -- Method
  "ƒ Function", -- Function
  " Constructor", -- Constructor
  "識 Field", -- Field
  " Variable", -- Variable
  " Class", -- Class
  "ﰮ Interface", -- Interface
  " Module", -- Module
  " Property", -- Property
  " Unit", -- Unit
  " Value", -- Value
  "了 Enum", -- Enum
  " Keyword", -- Keyword
  " Snippet", -- Snippet
  " Color", -- Color
  " File", -- File
  "渚 Reference", -- Reference
  " Folder", -- Folder
  " Enum", -- Enum
  " Constant", -- Constant
  " Struct", -- Struct
  "鬒 Event", -- Event
  " Operator", -- Operator
  " Type Parameter", -- TypeParameter
}

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
  underline = true,
  update_in_insert = false,
  virtual_text = { spacing = 4, prefix = "●" },
  severity_sort = true,
  signs = true,
})

local on_attach = function(client, bufnr)
  -- commands
  utils.lua_command("LspDef", "vim.lsp.buf.definition()")
  utils.lua_command("LspTypeDef", "vim.lsp.buf.type_definition()")
  utils.lua_command("LspImplementation", "vim.lsp.buf.implementation()")
  utils.lua_command("LspReferences", "vim.lsp.buf.references()")
  utils.lua_command("LspRename", "vim.lsp.buf.rename()")
  utils.lua_command("LspHover", "vim.lsp.buf.hover()")
  utils.lua_command("LspFormat", "vim.lsp.buf.formatting()")
  utils.command("LspLog", "edit " .. vim.lsp.get_log_path())

  -- mappings
  utils.buf_map("n", "gd", ":LspDef<CR>", nil, bufnr)
  utils.buf_map("n", "gD", ":LspTypeDef<CR>", nil, bufnr)
  utils.buf_map("n", "gi", ":LspImplementation<CR>", nil, bufnr)
  utils.buf_map("n", "gr", ":LspReferences<CR>", nil, bufnr)
  utils.buf_map("n", "rn", ":LspRename<CR>", nil, bufnr)
  utils.buf_map("n", "K", ":LspHover<CR>", nil, bufnr)
  utils.buf_map("n", "ca", ":Lspsaga code_action<CR>", nil, bufnr)
  utils.buf_map("n", "[d", ":Lspsaga diagnostic_jump_prev<CR>", nil, bufnr)
  utils.buf_map("n", "]d", ":Lspsaga diagnostic_jump_next<CR>", nil, bufnr)

  -- autocommands
  -- format-on-save
  if client.resolved_capabilities.document_formatting then
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

-- lspInstall

-- config that activates keymaps and enables snippet support
local function make_config()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  capabilities.textDocument.colorProvider = { dynamicRegistration = false }
  return {
    capabilities = capabilities,
    on_attach = on_attach,
    init_options = {},
    settings = {},
  }
end

local function setup_servers()
  require("lspinstall").setup()

  local lspconf = require("lspconfig")
  local servers = require("lspinstall").installed_servers()
  for _, lang in pairs(servers) do
    local config = make_config()
    if lang == "lua" then
      require("lsp.lua").setup(on_attach)
    elseif lang == "rust" then
      -- do nothing
      -- because rust-tools handles it all for us
    elseif lang == "typescript" then
      require("lsp.typescript").setup(on_attach)
    else
      if lang == "angular" then
        --
      elseif lang == "bash" then
        --
      elseif lang == "cpp" then
        local clangd_flags = { "--background-index" }
        config.cmd = { vim.fn.stdpath("data") .. "/lspinstall/cpp/clangd/bin/clangd", unpack(clangd_flags) }
        O.format_on_save = false
      elseif lang == "css" then
        --
      elseif lang == "deno" then
        --
      elseif lang == "html" then
        config.init_options = O.lang.html.init_options
      elseif lang == "json" then
        --
      elseif lang == "python" then
        --
      elseif lang == "vim" then
        config.init_options = { isNeovim = true }
      elseif lang == "vue" then
        --
      elseif lang == "yaml" then
        --
      end
      lspconf[lang].setup(config)
    end
  end
end

setup_servers()

-- :LspInstall post-install hook
require("lspinstall").post_install_hook = function()
  setup_servers() -- reload installed servers
  vim.cmd("bufdo e") -- triggers the FileType autocmd that starts the server
end

-- rust-analyzer is set up seperately because rust-tools
-- but rust-analyzer is in the arch repos, so not really a problem
require("lsp.rust").setup(on_attach)
-- null_ls has it's own thing
require("lsp.null-ls").setup(on_attach)
