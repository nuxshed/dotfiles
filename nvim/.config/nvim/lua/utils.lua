local api = vim.api
local format = string.format

local get_map_options = function(custom_options)
  local options = { noremap = true, silent = true }
  if custom_options then
    options = vim.tbl_extend("force", options, custom_options)
  end
  return options
end

local M = {}

-- to define mappings
M.map = function(mode, target, source, opts)
  api.nvim_set_keymap(mode, target, source, get_map_options(opts))
end

M.buf_map = function(mode, target, source, opts, bufnr)
  api.nvim_buf_set_keymap(bufnr, mode, target, source, get_map_options(opts))
end

-- autocommands

M.augroup = function(name, event, fn, ft)
  api.nvim_exec(
    format(
      [[
    augroup %s
        autocmd!
        autocmd %s %s %s
    augroup END
    ]],
      name,
      event,
      ft or "*",
      fn
    ),
    false
  )
end

M.buf_augroup = function(name, event, fn)
  api.nvim_exec(
    format(
      [[
    augroup %s
        autocmd! * <buffer>
        autocmd %s <buffer> %s
    augroup END
    ]],
      name,
      event,
      fn
    ),
    false
  )
end

-- used to define commands
function M.command(name, fn)
  vim.cmd(format("command! %s %s", name, fn))
end

function M.lua_command(name, fn)
  M.command(name, "lua " .. fn)
end

function M.is_buffer_empty()
  -- Check whether the current buffer is empty
  return vim.fn.empty(vim.fn.expand "%:t") == 1
end

function M.has_width_gt(cols)
  -- Check if the windows width is greater than a given number of columns
  return vim.fn.winwidth(0) / 2 > cols
end

function M.check_git_workspace()
  local get_git_dir = require("galaxyline.provider_vcs").get_git_dir
  if vim.bo.buftype == "terminal" then
    return false
  end
  local current_file = vim.fn.expand "%:p"
  local current_dir
  -- if file is a symlinks
  if vim.fn.getftype(current_file) == "link" then
    local real_file = vim.fn.resolve(current_file)
    current_dir = vim.fn.fnamemodify(real_file, ":h")
  else
    current_dir = vim.fn.expand "%:p:h"
  end
  local result = get_git_dir(current_dir)
  if not result then
    return false
  end
  return true
end

return M
