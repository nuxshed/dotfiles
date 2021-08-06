local utils = require "utils"
local map = utils.map
local wk = require "which-key"

wk.register({
  p = {
    name = "+packer",
    u = { "<CMD>PackerUpdate<CR>", "Update Plugins" },
    s = { "<CMD>PackerSync<CR>", "Sync Plugins" },
    i = { "<CMD>PackerInstall<CR>", "Install Plugins" },
    c = { "<CMD>PackerCompile<CR>", "Compile plugins.lua" },
  },
  [","] = { "<CMD>edit ~/.config/nvim/lua/config.lua<CR>", "edit config" },
  g = "lazygit",
  h = {
    name = "+gitsigns",
    hs = "stage hunk",
    hu = "undo stage hunk",
    hr = "reset hunk",
    hR = "reset buffer",
    hp = "preview hunk",
    hb = "blame line",
  },
}, {
  prefix = "<leader>",
})

-- clear search highlight on enter
map("n", "<CR>", [[ <Cmd> nohl<CR>]])

-- easily switch tabs
for i = 1, 9 do
  map(
    "n",
    "<A-" .. i .. ">",
    ':lua require"bufferline".go_to_buffer(' .. i .. ")<CR>"
  )
  wk.register {
    ["<A-" .. i .. ">"] = "switch to buffer " .. i,
  }
end
