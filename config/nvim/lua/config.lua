-- config file for my neovim configuration
-- meta, aint it?

O = {
  colorscheme = "zenbones",
  leader = " ",
  localleader = ",",
  borders = "single",
  format_on_save = true,
  icon_colors = false,
  smart_close = "help,startuptime,qf,lspinfo,packer,tsplayground", -- windows to close with 'q'
  icons = {
    kinds = {
      Class = "ﴯ ",
      Color = " ",
      Constant = "",
      Constructor = " ",
      Enum = "練",
      EnumMember = " ",
      Event = " ",
      Field = "識",
      File = "",
      Folder = " ",
      Function = " ",
      Interface = "ﰮ ",
      Keyword = " ",
      Method = " ",
      Module = " ",
      Operator = "",
      Property = " ",
      Reference = "渚",
      Snippet = " ",
      Struct = "פּ ",
      Text = " ",
      TypeParameter = " ",
      Unit = "塞",
      Value = " ",
      Variable = "",
    },
  },
  treesitter = {
    ensure_installed = {
      "bash",
      "c",
      "comment",
      "cpp",
      "css",
      "dart",
      "go",
      "html",
      "javascript",
      "json",
      "lua",
      -- "markdown",
      "python",
      "rust",
      "scss",
      "toml",
      "typescript",
      "tsx",
      "vue",
      "yaml",
    },
  },
  terminal = {
    direction = "horizontal",
  },
}

-- {{{ source project config
if vim.fn.filereadable(vim.fn.getcwd() .. "/.nvimrc.lua") == 1 then
  vim.cmd("luafile" .. vim.fn.getcwd() .. "/.nvimrc.lua")
end
-- }}}
