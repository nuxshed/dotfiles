local wk = require "which-key"

wk.register {
  ["<leader>"] = {
    -- toggle stuff
    t = {
      name = "+toggle",
      n = { "<CMD>set nu!<CR>", "number" },
      r = { "<CMD>set relativenumber!<CR>", "relative number" },
      i = { "<CMD>IndentBlanklineToggle<CR>", "indentline" },
      z = { "<CMD>ZenMode<CR>", "zen mode" },
      t = { "<CMD>Twilight<CR>", "twilight" },
      c = { "<CMD>ColorizerToggle<CR>", "colorizer" },
      f = { require("lsp.utils").toggle_format_on_save, "format on save" },
    },
    -- packer
    p = {
      name = "+packer",
      u = { "<CMD>PackerUpdate<CR>", "Update Plugins" },
      s = { "<CMD>PackerSync<CR>", "Sync Plugins" },
      i = { "<CMD>PackerInstall<CR>", "Install Plugins" },
      c = { "<CMD>PackerCompile<CR>", "Compile Config" },
      p = { "<CMD>PackerProfile<CR>", "Profile" },
    },
    [","] = { "<CMD>edit ~/.config/nvim/lua/config.lua<CR>", "edit config" },
    d = { "<CMD>TroubleToggle<CR>", "trouble" },
    -- git
    g = {
      name = "+git",
      s = "stage hunk",
      u = "undo stage hunk",
      r = "reset hunk",
      R = "reset buffer",
      p = "preview hunk",
      b = "blame line",
    },
    u = { "<CMD>UndotreeToggle<CR>", "undotree" },
  },

  -- localleader mappings
  ["<localleader>"] = {
    a = "swap: next",
    A = "swap: prev",
    t = { "<CMD>ciwtrue<ESC><CR>", "true" },
  },

  -- next
  ["]"] = {
    m = { "next function" },
  },

  -- prev
  ["["] = {
    m = { "prev function" },
  },

  ["<M-right>"] = { "<CMD>bn<CR>", "next buffer" },
  ["<M-left>"] = { "<CMD>bp<CR>", "prev buffer" },
}
