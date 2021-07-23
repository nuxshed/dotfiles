local M = {}

function M.setup(on_attach)
  require("lspconfig").json.setup {
    on_attach = on_attach,
    settings = {
      json = {
        schemas = {
          {
            fileMatch = { "package.json" },
            url = "https://json.schemastore.org/package.json",
          },
          {
            fileMatch = { "jsconfig*.json" },
            url = "https://json.schemastore.org/jsconfig.json",
          },
          {
            fileMatch = { "tsconfig*.json" },
            url = "https://json.schemastore.org/tsconfig.json",
          },
          {
            fileMatch = {
              ".prettierrc",
              ".prettierrc.json",
              "prettier.config.json",
            },
            url = "https://json.schemastore.org/prettierrc.json",
          },
          {
            fileMatch = { ".eslintrc", ".eslintrc.json" },
            url = "https://json.schemastore.org/eslintrc.json",
          },
          {
            fileMatch = { "nodemon.json" },
            url = "https://json.schemastore.org/nodemon.json",
          },
        },
      },
    },
  }
end

return M
