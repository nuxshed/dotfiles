local utils = require("utils")
local map = utils.map

map("n", "<leader>n", [[ <Cmd> set nu!<CR> ]])
map("n", "<leader>nr", [[ <Cmd> set relativenumber!<CR> ]])
map("n", "<leader>s", [[ <Cmd> set spell!<CR> ]])

-- clear search highlight on enter
map("n", "<CR>", [[ <Cmd> nohl<CR>]])

-- easily switch tabs
for i = 1, 9 do
	map("n", "<A-" .. i .. ">", ':lua require"bufferline".go_to_buffer(' .. i .. ")<CR>")
end

-- Packer Commands
map("n", "<leader>pu", [[ <Cmd> PackerUpdate<CR>]])
map("n", "<leader>pc", [[ <Cmd> PackerCompile<CR>]])
map("n", "<leader>ps", [[ <Cmd> PackerSync<CR>]])

-- lazygit
local Terminal = require("toggleterm.terminal").Terminal
local lazygit = Terminal:new({
	cmd = "lazygit",
	dir = "git_dir",
	direction = "float",
	float_opts = {
		border = "single",
	},
	on_open = function(term)
		vim.cmd("startinsert!")
		vim.api.nvim_buf_set_keymap(term.bufnr, "n", "q", "<cmd>close<CR>", { noremap = true, silent = true })
	end,
})

function _lazygit_toggle()
	lazygit:toggle()
end

-- vim.api.nvim_set_keymap("n", "<leader>g", "<cmd>lua _lazygit_toggle()<CR>", { noremap = true, silent = true })
map("n", "<leader>g", [[ <Cmd> lua _lazygit_toggle()<CR>]])
