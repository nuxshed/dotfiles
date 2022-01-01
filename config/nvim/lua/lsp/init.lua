local lspconfig = require "lspconfig"

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.documentationFormat = {
  "markdown",
}
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.preselectSupport = false
capabilities.textDocument.completion.completionItem.insertReplaceSupport = true
capabilities.textDocument.completion.completionItem.labelDetailsSupport = true
capabilities.textDocument.completion.completionItem.deprecatedSupport = true
capabilities.textDocument.completion.completionItem.commitCharactersSupport = true
capabilities.textDocument.completion.completionItem.tagSupport = {
  valueSet = { 1 },
}
capabilities.textDocument.completion.completionItem.resolveSupport = {
  properties = {
    "documentation",
    "detail",
    "additionalTextEdits",
  },
}

require "lsp.emmet"

local function on_attach(client, bufnr)
  require("lsp.autocommands").setup(client)
  require("lsp.commands").setup()
  require("lsp.mappings").setup(bufnr)
  require "lsp.handlers"
  require "lsp.signs"
end

local servers = {
  -- sumneko_lua = require("lsp.lang.lua").setup,
  html = { cmd = { "vscode-html-language-server", "--stdio" } },
  cssls = { cmd = { "vscode-css-language-server", "--stdio" } },
  rnix = {},
  pyright = {},
  rust_analyzer = {},
  ls_emmet = {},
}

require("lsp.null-ls").setup()

for name, opts in pairs(servers) do
  if type(opts) == "function" then
    opts()
  else
    local client = lspconfig[name]
    client.setup(vim.tbl_extend("force", {
      flags = { debounce_text_changes = 150 },
      on_attach = on_attach,
      capabilities = capabilities,
    }, opts))
  end
end
