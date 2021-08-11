local api = vim.api
local fmt = string.format

local function get_map_options(custom_options)
  local options = { noremap = true, silent = true }
  if custom_options then
    options = vim.tbl_extend("force", options, custom_options)
  end
  return options
end

local M = {}

-- to define mappings
function M.map(mode, target, source, opts)
  api.nvim_set_keymap(mode, target, source, get_map_options(opts))
end

function M.buf_map(mode, target, source, opts, bufnr)
  api.nvim_buf_set_keymap(bufnr, mode, target, source, get_map_options(opts))
end

-- autocommands

function M.augroup(name, commands)
  vim.cmd("augroup " .. name)
  vim.cmd "autocmd!"
  for _, c in ipairs(commands) do
    local command = c.command
    if type(command) == "function" then
      command = fmt "lua"
    end
    vim.cmd(
      fmt(
        "autocmd %s %s %s %s",
        table.concat(c.events, ","),
        table.concat(c.targets or {}, ","),
        table.concat(c.modifiers or {}, " "),
        command
      )
    )
  end
  vim.cmd "augroup END"
end

-- used to define commands
function M.command(name, fn)
  vim.cmd(fmt("command! %s %s", name, fn))
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

return M
