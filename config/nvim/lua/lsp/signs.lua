-- replace the default lsp diagnostic letters with prettier symbols
vim.fn.sign_define("DiagnosticSignError", { text = "", texthl = "LspDiagnosticsSignError" })
vim.fn.sign_define("DiagnosticSignWarn", { text = "", texthl = "LspDiagnosticsSignWarning" })
vim.fn.sign_define("DiagnosticSignInfo", { text = "", texthl = "LspDiagnosticsSignInformation" })
vim.fn.sign_define("DiagnosticSignHint", { text = "", texthl = "LspDiagnosticsSignHint" })

-- completion icons
require("vim.lsp.protocol").CompletionItemKind = O.icons.kinds
