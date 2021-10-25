-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

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
    config = { 'require("plugins.snippets")' },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/LuaSnip"
  },
  ["alpha-nvim"] = {
    config = { 'require"plugins.dashboard"' },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/alpha-nvim"
  },
  ["buftabline.nvim"] = {
    config = { 'require"plugins.buftabline"' },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/buftabline.nvim"
  },
  ["cmp-buffer"] = {
    after_files = { "/home/advait/.local/share/nvim/site/pack/packer/opt/cmp-buffer/after/plugin/cmp_buffer.lua" },
    load_after = {
      ["nvim-cmp"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/cmp-buffer"
  },
  ["cmp-nvim-lsp"] = {
    after_files = { "/home/advait/.local/share/nvim/site/pack/packer/opt/cmp-nvim-lsp/after/plugin/cmp_nvim_lsp.lua" },
    load_after = {
      ["nvim-lspconfig"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/cmp-nvim-lsp"
  },
  ["cmp-path"] = {
    after_files = { "/home/advait/.local/share/nvim/site/pack/packer/opt/cmp-path/after/plugin/cmp_path.lua" },
    load_after = {
      ["nvim-cmp"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/cmp-path"
  },
  ["cmp-spell"] = {
    after_files = { "/home/advait/.local/share/nvim/site/pack/packer/opt/cmp-spell/after/plugin/cmp-spell.lua" },
    load_after = {
      ["nvim-cmp"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/cmp-spell"
  },
  cmp_luasnip = {
    after_files = { "/home/advait/.local/share/nvim/site/pack/packer/opt/cmp_luasnip/after/plugin/cmp_luasnip.lua" },
    load_after = {
      ["nvim-cmp"] = true
    },
    loaded = false,
    needs_bufread = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/cmp_luasnip"
  },
  ["gitsigns.nvim"] = {
    config = { 'require("plugins.gitsigns")' },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/gitsigns.nvim"
  },
  ["gruvbox-material"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/gruvbox-material"
  },
  ["indent-blankline.nvim"] = {
    config = { "\27LJ\2\nœ\3\0\0\4\0\n\0\r6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0005\3\4\0=\3\5\0025\3\6\0=\3\a\0025\3\b\0=\3\t\2B\0\2\1K\0\1\0\21context_patterns\1\f\0\0\nclass\rfunction\vmethod\nblock\17list_literal\rselector\b^if\v^table\17if_statement\nwhile\bfor\20buftype_exclude\1\3\0\0\rterminal\vnofile\21filetype_exclude\1\r\0\0\14dashboard\blog\14gitcommit\vpacker\rmarkdown\btxt\thelp\rNvimTree\bgit\20TelescopePrompt\rundotree\5\1\0\4\18show_foldtext\1\tchar\6|\25show_current_context\2\28show_first_indent_level\2\nsetup\21indent_blankline\frequire\0" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/indent-blankline.nvim"
  },
  kommentary = {
    config = { "\27LJ\2\nx\0\0\4\0\5\0\b6\0\0\0'\2\1\0B\0\2\0029\0\2\0'\2\3\0005\3\4\0B\0\3\1K\0\1\0\1\0\1 prefer_single_line_comments\2\blua\23configure_language\22kommentary.config\frequire\0" },
    keys = { { "", "gc" }, { "", "gcc" } },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/kommentary"
  },
  ["markdown-preview.nvim"] = {
    config = { "\27LJ\2\nQ\0\0\2\0\4\0\t6\0\0\0009\0\1\0)\1\0\0=\1\2\0006\0\0\0009\0\1\0)\1\1\0=\1\3\0K\0\1\0\20mkdp_auto_close\20mkdp_auto_start\6g\bvim\0" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/markdown-preview.nvim"
  },
  neogit = {
    commands = { "Neogit" },
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/neogit"
  },
  ["nord.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nord.nvim"
  },
  ["null-ls.nvim"] = {
    load_after = {
      ["nvim-lspconfig"] = true
    },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/null-ls.nvim"
  },
  ["nvim-autopairs"] = {
    config = { "\27LJ\2\nX\0\0\3\0\4\0\a6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0B\0\2\1K\0\1\0\1\0\1\24close_triple_quotes\2\nsetup\19nvim-autopairs\frequire\0" },
    load_after = {
      ["nvim-cmp"] = true
    },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/nvim-autopairs"
  },
  ["nvim-cmp"] = {
    after = { "cmp-buffer", "cmp-path", "cmp-spell", "cmp_luasnip", "nvim-autopairs" },
    config = { 'require "plugins.cmp"' },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/nvim-cmp",
    wants = { "LuaSnip" }
  },
  ["nvim-colorizer.lua"] = {
    commands = { "ColorizerToggle" },
    keys = { { "", "<leader>tc" } },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/nvim-colorizer.lua"
  },
  ["nvim-lspconfig"] = {
    after = { "null-ls.nvim", "trouble.nvim", "cmp-nvim-lsp" },
    config = { 'require "lsp"' },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/nvim-lspconfig"
  },
  ["nvim-toggleterm.lua"] = {
    commands = { "ToggleTerm" },
    config = { 'require"plugins.terminal"' },
    keys = { { "", "<A-`>" } },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/nvim-toggleterm.lua"
  },
  ["nvim-tree.lua"] = {
    commands = { "NvimTreeToggle" },
    config = { 'require"plugins.nvim-tree"' },
    keys = { { "", "<C-n>" } },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    config = { 'require "plugins.treesitter"' },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-treesitter-textobjects"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nvim-treesitter-textobjects"
  },
  ["nvim-ts-autotag"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nvim-ts-autotag"
  },
  ["nvim-ts-context-commentstring"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/nvim-ts-context-commentstring"
  },
  ["nvim-web-devicons"] = {
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/nvim-web-devicons"
  },
  ["onedark.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/onedark.nvim"
  },
  ["packer.nvim"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/packer.nvim"
  },
  playground = {
    commands = { "TSPlaygroundToggle", "TSHighlightCapturesUnderCursor" },
    config = { "\27LJ\2\n¬\1\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\1K\0\1\0\19<localleader>s\1\0\0\1\3\0\0,<CMD>TSHighlightCapturesUnderCursor<CR>,syntax highlight group under the cursor\rregister\14which-key\frequire\0" },
    keys = { { "", "<localleader>s" } },
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/playground"
  },
  ["plenary.nvim"] = {
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/plenary.nvim"
  },
  ["project.nvim"] = {
    commands = { "ProjectRoot" },
    config = { "\27LJ\2\ný\1\0\0\4\0\b\0\v6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0005\3\4\0=\3\5\0025\3\6\0=\3\a\2B\0\2\1K\0\1\0\rpatterns\1\14\0\0\16stylua.toml\vrc.lua\15config.org\rMakefile\17package.json\15Cargo.toml\t.git\v_darcs\b.hg\t.bzr\t.svn\15index.html\14!^.config\15ignore_lsp\1\2\0\0\fnull-ls\1\0\2\16manual_mode\2\16show_hidden\2\nsetup\17project_nvim\frequire\0" },
    keys = { { "", "<leader>fp" } },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/project.nvim"
  },
  serenade = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/serenade"
  },
  ["sqlite.lua"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/sqlite.lua"
  },
  tabular = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/tabular"
  },
  ["telescope-frecency.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/telescope-frecency.nvim"
  },
  ["telescope-fzf-native.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/telescope-fzf-native.nvim"
  },
  ["telescope.nvim"] = {
    config = { 'require"plugins.telescope"' },
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/telescope.nvim",
    wants = { "project.nvim" }
  },
  ["todo-comments.nvim"] = {
    config = { "\27LJ\2\nñ\2\0\0\5\0\16\0\0196\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0005\3\5\0005\4\4\0=\4\6\0035\4\a\0=\4\b\0035\4\t\0=\4\n\0035\4\v\0=\4\f\0035\4\r\0=\4\14\3=\3\15\2B\0\2\1K\0\1\0\vcolors\fdefault\1\3\0\0\15Identifier\f#7daea3\thint\1\3\0\0\30LspDiagnosticsDefaultHint\f#89b482\tinfo\1\3\0\0%LspDiagnosticsDefaultInformation\f#a9b665\fwarning\1\4\0\0!LspDiagnosticsDefaultWarning\15WarningMsg\f#d8a657\nerror\1\0\0\1\4\0\0\31LspDiagnosticsDefaultError\rErrorMsg\f#ea6962\1\0\1\nsigns\1\nsetup\18todo-comments\frequire\0" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/todo-comments.nvim"
  },
  ["tokyonight.nvim"] = {
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/tokyonight.nvim"
  },
  ["trouble.nvim"] = {
    commands = { "Trouble", "TroubleToggle" },
    config = { "\27LJ\2\nH\0\0\3\0\4\0\a6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0B\0\2\1K\0\1\0\1\0\1\15auto_close\2\nsetup\ftrouble\frequire\0" },
    load_after = {
      ["nvim-lspconfig"] = true
    },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/trouble.nvim"
  },
  ["twilight.nvim"] = {
    commands = { "Twilight" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/twilight.nvim"
  },
  undotree = {
    commands = { "UndotreeToggle" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/undotree"
  },
  ["vim-log-highlighting"] = {
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/vim-log-highlighting"
  },
  ["vim-markdown"] = {
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/vim-markdown"
  },
  ["which-key.nvim"] = {
    config = { "\27LJ\2\nn\0\0\5\0\b\0\v6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\6\0005\3\4\0005\4\3\0=\4\5\3=\3\a\2B\0\2\1K\0\1\0\fplugins\1\0\0\rspelling\1\0\0\1\0\1\fenabled\2\nsetup\14which-key\frequire\0" },
    loaded = true,
    path = "/home/advait/.local/share/nvim/site/pack/packer/start/which-key.nvim"
  },
  ["zen-mode.nvim"] = {
    commands = { "ZenMode" },
    config = { "\27LJ\2\n‘\1\0\0\4\0\n\0\r6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\0025\3\6\0=\3\a\0025\3\b\0=\3\t\2B\0\2\1K\0\1\0\nkitty\1\0\1\fenabled\2\rgitsigns\1\0\1\fenabled\2\rtwilight\1\0\0\1\0\1\fenabled\1\nsetup\rzen-mode\frequire\0" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/advait/.local/share/nvim/site/pack/packer/opt/zen-mode.nvim"
  }
}

time([[Defining packer_plugins]], false)
local module_lazy_loads = {
  ["^gitsigns"] = "gitsigns.nvim",
  ["^neogit"] = "neogit",
  ["^null%-ls"] = "null-ls.nvim",
  ["^nvim%-autopairs"] = "nvim-autopairs",
  ["^nvim%-web%-devicons"] = "nvim-web-devicons",
  ["plenary.*"] = "plenary.nvim"
}
local lazy_load_called = {['packer.load'] = true}
local function lazy_load_module(module_name)
  local to_load = {}
  if lazy_load_called[module_name] then return nil end
  lazy_load_called[module_name] = true
  for module_pat, plugin_name in pairs(module_lazy_loads) do
    if not _G.packer_plugins[plugin_name].loaded and string.match(module_name, module_pat) then
      to_load[#to_load + 1] = plugin_name
    end
  end

  if #to_load > 0 then
    require('packer.load')(to_load, {module = module_name}, _G.packer_plugins)
    local loaded_mod = package.loaded[module_name]
    if loaded_mod then
      return function(modname) return loaded_mod end
    end
  end
end

if not vim.g.packer_custom_loader_enabled then
  table.insert(package.loaders, 1, lazy_load_module)
  vim.g.packer_custom_loader_enabled = true
end

-- Config for: which-key.nvim
time([[Config for which-key.nvim]], true)
try_loadstring("\27LJ\2\nn\0\0\5\0\b\0\v6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\6\0005\3\4\0005\4\3\0=\4\5\3=\3\a\2B\0\2\1K\0\1\0\fplugins\1\0\0\rspelling\1\0\0\1\0\1\fenabled\2\nsetup\14which-key\frequire\0", "config", "which-key.nvim")
time([[Config for which-key.nvim]], false)
-- Config for: nvim-treesitter
time([[Config for nvim-treesitter]], true)
require "plugins.treesitter"
time([[Config for nvim-treesitter]], false)

-- Command lazy-loads
time([[Defining lazy-load commands]], true)
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Neogit lua require("packer.load")({'neogit'}, { cmd = "Neogit", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file UndotreeToggle lua require("packer.load")({'undotree'}, { cmd = "UndotreeToggle", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file NvimTreeToggle lua require("packer.load")({'nvim-tree.lua'}, { cmd = "NvimTreeToggle", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file TSHighlightCapturesUnderCursor lua require("packer.load")({'playground'}, { cmd = "TSHighlightCapturesUnderCursor", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file ToggleTerm lua require("packer.load")({'nvim-toggleterm.lua'}, { cmd = "ToggleTerm", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Trouble lua require("packer.load")({'trouble.nvim'}, { cmd = "Trouble", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file TroubleToggle lua require("packer.load")({'trouble.nvim'}, { cmd = "TroubleToggle", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file ZenMode lua require("packer.load")({'zen-mode.nvim'}, { cmd = "ZenMode", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file ColorizerToggle lua require("packer.load")({'nvim-colorizer.lua'}, { cmd = "ColorizerToggle", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file ProjectRoot lua require("packer.load")({'project.nvim'}, { cmd = "ProjectRoot", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Twilight lua require("packer.load")({'twilight.nvim'}, { cmd = "Twilight", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file TSPlaygroundToggle lua require("packer.load")({'playground'}, { cmd = "TSPlaygroundToggle", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
time([[Defining lazy-load commands]], false)

-- Keymap lazy-loads
time([[Defining lazy-load keymaps]], true)
vim.cmd [[noremap <silent> <leader>tc <cmd>lua require("packer.load")({'nvim-colorizer.lua'}, { keys = "<lt>leader>tc", prefix = "" }, _G.packer_plugins)<cr>]]
vim.cmd [[noremap <silent> gcc <cmd>lua require("packer.load")({'kommentary'}, { keys = "gcc", prefix = "" }, _G.packer_plugins)<cr>]]
vim.cmd [[noremap <silent> <localleader>s <cmd>lua require("packer.load")({'playground'}, { keys = "<lt>localleader>s", prefix = "" }, _G.packer_plugins)<cr>]]
vim.cmd [[noremap <silent> <A-`> <cmd>lua require("packer.load")({'nvim-toggleterm.lua'}, { keys = "<lt>A-`>", prefix = "" }, _G.packer_plugins)<cr>]]
vim.cmd [[noremap <silent> <leader>fp <cmd>lua require("packer.load")({'project.nvim'}, { keys = "<lt>leader>fp", prefix = "" }, _G.packer_plugins)<cr>]]
vim.cmd [[noremap <silent> <C-n> <cmd>lua require("packer.load")({'nvim-tree.lua'}, { keys = "<lt>C-n>", prefix = "" }, _G.packer_plugins)<cr>]]
vim.cmd [[noremap <silent> gc <cmd>lua require("packer.load")({'kommentary'}, { keys = "gc", prefix = "" }, _G.packer_plugins)<cr>]]
time([[Defining lazy-load keymaps]], false)

vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
time([[Defining lazy-load filetype autocommands]], true)
vim.cmd [[au FileType log ++once lua require("packer.load")({'vim-log-highlighting'}, { ft = "log" }, _G.packer_plugins)]]
vim.cmd [[au FileType markdown ++once lua require("packer.load")({'markdown-preview.nvim', 'vim-markdown'}, { ft = "markdown" }, _G.packer_plugins)]]
time([[Defining lazy-load filetype autocommands]], false)
  -- Event lazy-loads
time([[Defining lazy-load event autocommands]], true)
vim.cmd [[au BufReadPre * ++once lua require("packer.load")({'nvim-lspconfig'}, { event = "BufReadPre *" }, _G.packer_plugins)]]
vim.cmd [[au VimEnter * ++once lua require("packer.load")({'alpha-nvim', 'telescope.nvim'}, { event = "VimEnter *" }, _G.packer_plugins)]]
vim.cmd [[au BufRead * ++once lua require("packer.load")({'gitsigns.nvim', 'indent-blankline.nvim', 'todo-comments.nvim'}, { event = "BufRead *" }, _G.packer_plugins)]]
vim.cmd [[au InsertEnter * ++once lua require("packer.load")({'LuaSnip', 'nvim-cmp'}, { event = "InsertEnter *" }, _G.packer_plugins)]]
vim.cmd [[au BufEnter * ++once lua require("packer.load")({'buftabline.nvim'}, { event = "BufEnter *" }, _G.packer_plugins)]]
time([[Defining lazy-load event autocommands]], false)
vim.cmd("augroup END")
vim.cmd [[augroup filetypedetect]]
time([[Sourcing ftdetect script at: /home/advait/.local/share/nvim/site/pack/packer/opt/vim-log-highlighting/ftdetect/log.vim]], true)
vim.cmd [[source /home/advait/.local/share/nvim/site/pack/packer/opt/vim-log-highlighting/ftdetect/log.vim]]
time([[Sourcing ftdetect script at: /home/advait/.local/share/nvim/site/pack/packer/opt/vim-log-highlighting/ftdetect/log.vim]], false)
time([[Sourcing ftdetect script at: /home/advait/.local/share/nvim/site/pack/packer/opt/vim-markdown/ftdetect/markdown.vim]], true)
vim.cmd [[source /home/advait/.local/share/nvim/site/pack/packer/opt/vim-markdown/ftdetect/markdown.vim]]
time([[Sourcing ftdetect script at: /home/advait/.local/share/nvim/site/pack/packer/opt/vim-markdown/ftdetect/markdown.vim]], false)
vim.cmd("augroup END")
if should_profile then save_profiles(0) end

end)

if not no_errors then
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
