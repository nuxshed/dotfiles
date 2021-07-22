local lspconfig = require("lspconfig")
local lspinstall = require("lspinstall")
local utils = require("utils")

local function on_attach(client, bufnr)
  require("lsp.commands").setup()
  require("lsp.autocommands").setup(client)
  require("lsp.mappings").setup(bufnr)
  require("lsp.formatting").setup(client, bufnr)
end

require("lsp.diagnostics")
require("lsp.signs")

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

-- lspInstall + lspconfig stuff

local function setup_servers()
  lspinstall.setup()
  local servers = lspinstall.installed_servers()

  for _, lang in pairs(servers) do
    if lang == "lua" then
      require("lsp.lang.lua").setup(on_attach)
    elseif lang == "rust" then
      if not O.lang.rust.rust_tools.enabled then
        require("lsp.lang.rust").setup(on_attach)
      end
    elseif lang == "typescript" then
      require("lsp.lang.typescript").setup(on_attach)
    else
      local config = make_config()
      lspconfig[lang].setup(config)
    end
  end
end

setup_servers()

-- Automatically reload after `:LspInstall <server>` so we don't have to restart neovim
lspinstall.post_install_hook = function()
  setup_servers() -- reload installed servers
  vim.cmd("bufdo e") -- triggers FileType autocmd that starts the server
end

-- null-ls
require("lsp.null-ls").setup(on_attach)

return on_attach
