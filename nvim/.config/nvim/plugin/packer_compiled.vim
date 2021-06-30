" Automatically generated packer.nvim plugin loader code

if !has('nvim-0.5')
  echohl WarningMsg
  echom "Invalid Neovim version for packer.nvim!"
  echohl None
  finish
endif

packadd packer.nvim

try

lua << END
  local time
  local profile_info
  local should_profile = true
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/home/advait/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/advait/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/advait/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/advait/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/advait/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  LuaSnip = {
    config = { "\27LJ\2\n�\1\0\0\3\0\a\0\r6\0\0\0'\2\1\0B\0\2\0029\0\2\0009\0\3\0005\2\4\0B\0\2\0016\0\0\0'\2\5\0B\0\2\0029\0\6\0B\0\1\1K\0\1\0\tload luasnip/loaders/from_vscode\1\0\2\17updateevents\29TextChanged,TextChangedI\fhistory\2\15set_config\vconfig\fluasnip\frequire\0" },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/LuaSnip"
  },
  ["codi.vim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/codi.vim"
  },
  ["dashboard-nvim"] = {
    config = { 'require("plugins.dashboard")' },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/dashboard-nvim"
  },
  ["friendly-snippets"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/friendly-snippets"
  },
  ["galaxyline.nvim"] = {
    config = { 'require("plugins.statusline")' },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/galaxyline.nvim"
  },
  ["gitsigns.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/gitsigns.nvim"
  },
  ["hop.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/hop.nvim"
  },
  ["indent-blankline.nvim"] = {
    config = { 'require("plugins.indentline")' },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/indent-blankline.nvim"
  },
  ["lsp-status.nvim"] = {
    config = { "\27LJ\2\n�\1\0\0\4\0\5\0\t6\0\0\0'\2\1\0B\0\2\0029\1\2\0005\3\3\0B\1\2\0019\1\4\0B\1\1\1K\0\1\0\22register_progress\1\0\5\21indicator_errors\b✗\23indicator_warnings\b\19indicator_info\b\19indicator_hint\b\18status_symbol\t \vconfig\15lsp-status\frequire\0" },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/lsp-status.nvim"
  },
  ["lspkind-nvim"] = {
    config = { 'require("lspkind").init()' },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/lspkind-nvim"
  },
  ["lspsaga.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/lspsaga.nvim"
  },
  ["lua-dev.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/lua-dev.nvim"
  },
  ["markdown-preview.nvim"] = {
    commands = { "MarkdownPreview" },
    loaded = false,
    needs_bufread = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/markdown-preview.nvim"
  },
  ["neoscroll.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/neoscroll.nvim"
  },
  ["null-ls.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/null-ls.nvim"
  },
  ["nvim-autopairs"] = {
    config = { 'require("nvim-autopairs").setup()' },
    loaded = false,
    needs_bufread = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/nvim-autopairs"
  },
  ["nvim-base16.lua"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nvim-base16.lua"
  },
  ["nvim-bufferline.lua"] = {
    config = { 'require("plugins.bufferline")' },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nvim-bufferline.lua"
  },
  ["nvim-colorizer.lua"] = {
    config = { "require'colorizer'.setup()" },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nvim-colorizer.lua"
  },
  ["nvim-comment"] = {
    config = { "\27LJ\2\n:\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\17nvim_comment\frequire\0" },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nvim-comment"
  },
  ["nvim-compe"] = {
    after_files = { "/home/advait/.local/share/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe.vim" },
    config = { 'require("plugins.compe")' },
    loaded = false,
    needs_bufread = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/nvim-compe"
  },
  ["nvim-lsp-ts-utils"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/nvim-lsp-ts-utils"
  },
  ["nvim-lspconfig"] = {
    config = { 'require("lsp.config")' },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nvim-lspconfig"
  },
  ["nvim-lspinstall"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nvim-lspinstall"
  },
  ["nvim-luaref"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nvim-luaref"
  },
  ["nvim-toggleterm.lua"] = {
    config = { 'require("plugins.terminal")' },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nvim-toggleterm.lua"
  },
  ["nvim-tree.lua"] = {
    config = { 'require("plugins.nvim-tree")' },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    config = { 'require("plugins.treesitter")' },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-web-devicons"] = {
    config = { 'require("plugins.devicons")' },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/popup.nvim"
  },
  ["startuptime.vim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/startuptime.vim"
  },
  ["symbols-outline.nvim"] = {
    commands = { "SymbolsOutline", "SymbolsOutlineOpen", "SymbolsOutlineClose" },
    loaded = false,
    needs_bufread = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/symbols-outline.nvim"
  },
  tabular = {
    after_files = { "/home/advait/.local/share/nvim/site/pack/packer/opt/tabular/after/plugin/TabularMaps.vim" },
    load_after = {
      ["vim-markdown"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/tabular"
  },
  ["telescope.nvim"] = {
    config = { 'require("plugins.telescope")' },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/telescope.nvim"
  },
  ["todo-comments.nvim"] = {
    config = { 'require("plugins.todo-comments")' },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/todo-comments.nvim"
  },
  ["trouble.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/trouble.nvim"
  },
  ["vim-markdown"] = {
    after = { "tabular" },
    loaded = false,
    needs_bufread = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/vim-markdown"
  },
  ["vim-toml"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/vim-toml"
  },
  ["which-key.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/which-key.nvim"
  },
  ["zen-mode.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/zen-mode.nvim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: nvim-colorizer.lua
time([[Config for nvim-colorizer.lua]], true)
require'colorizer'.setup()
time([[Config for nvim-colorizer.lua]], false)
-- Config for: nvim-web-devicons
time([[Config for nvim-web-devicons]], true)
require("plugins.devicons")
time([[Config for nvim-web-devicons]], false)
-- Config for: nvim-treesitter
time([[Config for nvim-treesitter]], true)
require("plugins.treesitter")
time([[Config for nvim-treesitter]], false)
-- Config for: dashboard-nvim
time([[Config for dashboard-nvim]], true)
require("plugins.dashboard")
time([[Config for dashboard-nvim]], false)
-- Config for: nvim-toggleterm.lua
time([[Config for nvim-toggleterm.lua]], true)
require("plugins.terminal")
time([[Config for nvim-toggleterm.lua]], false)
-- Config for: telescope.nvim
time([[Config for telescope.nvim]], true)
require("plugins.telescope")
time([[Config for telescope.nvim]], false)
-- Config for: nvim-bufferline.lua
time([[Config for nvim-bufferline.lua]], true)
require("plugins.bufferline")
time([[Config for nvim-bufferline.lua]], false)
-- Config for: indent-blankline.nvim
time([[Config for indent-blankline.nvim]], true)
require("plugins.indentline")
time([[Config for indent-blankline.nvim]], false)
-- Config for: todo-comments.nvim
time([[Config for todo-comments.nvim]], true)
require("plugins.todo-comments")
time([[Config for todo-comments.nvim]], false)
-- Config for: nvim-comment
time([[Config for nvim-comment]], true)
try_loadstring("\27LJ\2\n:\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\17nvim_comment\frequire\0", "config", "nvim-comment")
time([[Config for nvim-comment]], false)
-- Config for: galaxyline.nvim
time([[Config for galaxyline.nvim]], true)
require("plugins.statusline")
time([[Config for galaxyline.nvim]], false)
-- Config for: lspkind-nvim
time([[Config for lspkind-nvim]], true)
require("lspkind").init()
time([[Config for lspkind-nvim]], false)
-- Config for: nvim-tree.lua
time([[Config for nvim-tree.lua]], true)
require("plugins.nvim-tree")
time([[Config for nvim-tree.lua]], false)
-- Config for: LuaSnip
time([[Config for LuaSnip]], true)
try_loadstring("\27LJ\2\n�\1\0\0\3\0\a\0\r6\0\0\0'\2\1\0B\0\2\0029\0\2\0009\0\3\0005\2\4\0B\0\2\0016\0\0\0'\2\5\0B\0\2\0029\0\6\0B\0\1\1K\0\1\0\tload luasnip/loaders/from_vscode\1\0\2\17updateevents\29TextChanged,TextChangedI\fhistory\2\15set_config\vconfig\fluasnip\frequire\0", "config", "LuaSnip")
time([[Config for LuaSnip]], false)
-- Config for: lsp-status.nvim
time([[Config for lsp-status.nvim]], true)
try_loadstring("\27LJ\2\n�\1\0\0\4\0\5\0\t6\0\0\0'\2\1\0B\0\2\0029\1\2\0005\3\3\0B\1\2\0019\1\4\0B\1\1\1K\0\1\0\22register_progress\1\0\5\21indicator_errors\b✗\23indicator_warnings\b\19indicator_info\b\19indicator_hint\b\18status_symbol\t \vconfig\15lsp-status\frequire\0", "config", "lsp-status.nvim")
time([[Config for lsp-status.nvim]], false)
-- Config for: nvim-lspconfig
time([[Config for nvim-lspconfig]], true)
require("lsp.config")
time([[Config for nvim-lspconfig]], false)

-- Command lazy-loads
time([[Defining lazy-load commands]], true)
vim.cmd [[command! -nargs=* -range -bang -complete=file SymbolsOutlineOpen lua require("packer.load")({'symbols-outline.nvim'}, { cmd = "SymbolsOutlineOpen", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file SymbolsOutlineClose lua require("packer.load")({'symbols-outline.nvim'}, { cmd = "SymbolsOutlineClose", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file MarkdownPreview lua require("packer.load")({'markdown-preview.nvim'}, { cmd = "MarkdownPreview", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file SymbolsOutline lua require("packer.load")({'symbols-outline.nvim'}, { cmd = "SymbolsOutline", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
time([[Defining lazy-load commands]], false)

vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
time([[Defining lazy-load filetype autocommands]], true)
vim.cmd [[au FileType typescript ++once lua require("packer.load")({'nvim-lsp-ts-utils'}, { ft = "typescript" }, _G.packer_plugins)]]
vim.cmd [[au FileType markdown ++once lua require("packer.load")({'vim-markdown', 'markdown-preview.nvim'}, { ft = "markdown" }, _G.packer_plugins)]]
time([[Defining lazy-load filetype autocommands]], false)
  -- Event lazy-loads
time([[Defining lazy-load event autocommands]], true)
vim.cmd [[au InsertEnter * ++once lua require("packer.load")({'nvim-compe'}, { event = "InsertEnter *" }, _G.packer_plugins)]]
vim.cmd [[au BufRead * ++once lua require("packer.load")({'nvim-autopairs'}, { event = "BufRead *" }, _G.packer_plugins)]]
time([[Defining lazy-load event autocommands]], false)
vim.cmd("augroup END")
vim.cmd [[augroup filetypedetect]]
time([[Sourcing ftdetect script at: /home/advait/.local/share/nvim/site/pack/packer/opt/vim-markdown/ftdetect/markdown.vim]], true)
vim.cmd [[source /home/advait/.local/share/nvim/site/pack/packer/opt/vim-markdown/ftdetect/markdown.vim]]
time([[Sourcing ftdetect script at: /home/advait/.local/share/nvim/site/pack/packer/opt/vim-markdown/ftdetect/markdown.vim]], false)
vim.cmd("augroup END")
if should_profile then save_profiles(1) end

END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry
