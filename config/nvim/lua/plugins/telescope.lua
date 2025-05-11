local telescope = require "telescope"
local actions = require "telescope.actions"
local builtin = require "telescope.builtin"
local extensions = telescope.extensions
local load_extension = telescope.load_extension

telescope.setup {
  defaults = {
    mappings = {
      i = {
        ["<c-c>"] = function()
          vim.cmd "stopinsert!"
        end,
        ["<esc>"] = actions.close,
        ["<c-s>"] = actions.select_horizontal,
        ["<c-j>"] = actions.cycle_history_next,
        ["<c-k>"] = actions.cycle_history_prev,
      },
    },
    file_ignore_patterns = {
      "%.jpg",
      "%.jpeg",
      "%.png",
      "%.otf",
      "%.ttf",
      "node_modules",
      ".git",
    },
    layout_strategy = "flex",
    layout_config = {
      horizontal = {
        preview_width = 0.6,
      },
    },
    -- winblend = 30,
    border = {},
    borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
    extensions = {
      -- frecency = {
      --   workspaces = {
      --     dotfiles = "$HOME/dotfiles",
      --     projects = "$HOME/projects",
      --     dwm = "$HOME/projects/dwm",
      --     site = "$HOME/projects/site",
      --   },
      -- },
    },
  },
}

load_extension "file_browser"
load_extension "live_grep_args"

local function grep()
  extensions.live_grep_args.live_grep_args {
    path_display = { "shorten" },
    only_sort_text = true,
    use_regex = true,
  }
end

local function ivy()
  vim.cmd [[ set laststatus=0 ]]
  builtin.find_files {
    previewer = false,
    sorting_strategy = "ascending",
    preview_title = "",
    layout_strategy = "bottom_pane",
    layout_config = {
      height = 8,
    },
    border = true,
    borderchars = {
      "z",
      prompt = { "─", " ", " ", " ", "─", "─", " ", " " },
      results = { " " },
    },
  }
  vim.cmd [[ set laststatus=2 ]]
end

local function browse_files()
  vim.cmd [[ set laststatus=0 ]]
  extensions.file_browser.file_browser {
    previewer = false,
    sorting_strategy = "ascending",
    preview_title = "",
    layout_strategy = "bottom_pane",
    layout_config = {
      height = 10,
    },
    border = true,
    borderchars = { "" },
  }
  vim.cmd [[ set laststatus=2 ]]
end

local function buffers()
  vim.cmd [[ set laststatus=0 ]]
  builtin.buffers {
    previewer = false,
    sorting_strategy = "ascending",
    preview_title = "",
    layout_strategy = "bottom_pane",
    layout_config = {
      height = 7,
    },
    border = true,
    borderchars = {
      "z",
      prompt = { "─", " ", " ", " ", "─", "─", " ", " " },
      results = { " " },
    },
  }
  vim.cmd [[ set laststatus=2 ]]
end

local function lsp_code_actions()
  builtin.lsp_code_actions {
    sorting_strategy = "ascending",
    results_title = false,
    layout_strategy = "cursor",
    layout_config = {
      width = 70,
      height = 10,
    },
    borderchars = {
      prompt = { "─", "│", " ", "│", "┌", "┐", "│", "│" },
      results = { "─", "│", "─", "│", "├", "┤", "┘", "└" },
      preview = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
    },
  }
end

local wk = require "which-key"

wk.add {
  {
    "<M-b>",
    function()
      browse_files()
    end,
    desc = "browse files",
  },
  {
    "<M-f>",
    function()
      ivy()
    end,
    desc = "find files",
  },
  {
    "<M-x>",
    function()
      buffers()
    end,
    desc = "switch buffer",
  },
  { "<leader>f", group = "+find" },
  {
    "<leader>fb",
    function()
      builtin.current_buffer_fuzzy_find()
    end,
    desc = "find in buffer",
  },
  {
    "<leader>ff",
    function()
      builtin.find_files()
    end,
    desc = "files",
  },
  { "<leader>fg", group = "+git" },
  {
    "<leader>fgS",
    function()
      builtin.git_stash()
    end,
    desc = "stash",
  },
  {
    "<leader>fgb",
    function()
      builtin.git_branches()
    end,
    desc = "branches",
  },
  {
    "<leader>fgc",
    function()
      builtin.git_commits()
    end,
    desc = "commits",
  },
  {
    "<leader>fgf",
    function()
      builtin.git_files()
    end,
    desc = "files",
  },
  {
    "<leader>fgs",
    function()
      builtin.git_status()
    end,
    desc = "status",
  },
  {
    "<leader>fh",
    function()
      builtin.help_tags()
    end,
    desc = "help",
  },
  {
    "<leader>fi",
    function()
      ivy()
    end,
    desc = "ivy",
  },
  { "<leader>fl", group = "+lsp" },
  {
    "<leader>fld",
    function()
      builtin.lsp_definitions()
    end,
    desc = "definitions",
  },
  {
    "<leader>fli",
    function()
      builtin.lsp_implementations()
    end,
    desc = "implementations",
  },
  {
    "<leader>fls",
    function()
      builtin.lsp_document_symbols()
    end,
    desc = "document symbols",
  },
  {
    "<leader>flw",
    function()
      builtin.lsp_workspace_symbols()
    end,
    desc = "workspace symbols",
  },
  {
    "<leader>fw",
    function()
      grep()
    end,
    desc = "grep",
  },
}

return { lsp_code_actions = lsp_code_actions }
