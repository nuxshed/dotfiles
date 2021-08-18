local utils = require "utils"
local map = utils.map
local wk = require "which-key"

wk.register({
  t = {
    name = "+toggle",
    n = { "<CMD>set nu!<CR>", "number" },
    r = { "<CMD>set relativenumber!<CR>", "relative number" },
    i = { "<CMD>IndentBlanklineToggle<CR>", "indentline" },
    z = { "<CMD>ZenMode<CR>", "zen mode" },
  },
  p = {
    name = "+packer",
    u = { "<CMD>PackerUpdate<CR>", "Update Plugins" },
    s = { "<CMD>PackerSync<CR>", "Sync Plugins" },
    i = { "<CMD>PackerInstall<CR>", "Install Plugins" },
    c = { "<CMD>PackerCompile<CR>", "Compile plugins.lua" },
    p = {"<CMD>PackerProfile<CR>", "Profile"}
  },
  [","] = { "<CMD>edit ~/.config/nvim/lua/config.lua<CR>", "edit config" },
  g = "lazygit",
  h = {
    name = "+gitsigns",
    s = "stage hunk",
    u = "undo stage hunk",
    r = "reset hunk",
    R = "reset buffer",
    p = "preview hunk",
    b = "blame line",
  },
  b = "blame line",
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
