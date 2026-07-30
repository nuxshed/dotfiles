local wk = require "which-key"

wk.setup {}

wk.add {
  -- toggle
  { "<leader>t", group = "toggle" },
  { "<leader>tn", "<cmd>set nu!<cr>", desc = "numbers" },
  { "<leader>tr", "<cmd>set relativenumber!<cr>", desc = "relativenumber" },
  { "<leader>tw", "<cmd>set wrap!<cr>", desc = "wrap" },
  { "<leader>tz", "<cmd>ZenMode<cr>", desc = "zen mode" },
  { "<leader>tt", "<cmd>Twilight<cr>", desc = "twilight" },
  { "<leader>tc", "<cmd>ColorizerToggle<cr>", desc = "colorizer" },
  { "<leader>tp", "<cmd>Precognition toggle<cr>", desc = "precognition" },

  -- lazy
  { "<leader>p", group = "lazy" },
  { "<leader>pi", "<cmd>Lazy install<cr>", desc = "install" },
  { "<leader>ps", "<cmd>Lazy sync<cr>", desc = "sync" },
  { "<leader>pu", "<cmd>Lazy update<cr>", desc = "update" },
  { "<leader>pc", "<cmd>Lazy clean<cr>", desc = "clean" },

  -- git
  { "<leader>g", group = "git" },

  -- format
  { "<leader>x", '<cmd>lua require("conform").format()<cr>', desc = "format" },
}
