local api = vim.api
local fmt = string.format

_G.utils = {
  _functions = {},
}

-- USAGE
-- utils.inspect(utils._functions)
-- or
-- :lua utils.inspect(utils._functions)
---@vararg any
function utils.inspect(...)
  local objects, v = {}, nil
  for i = 1, select("#", ...) do
    v = select(i, ...)
    table.insert(objects, vim.inspect(v))
  end

  print(table.concat(objects, "\n"))
  return ...
end

function utils.create(f)
  table.insert(utils._functions, f)
  return #utils._functions
end

function utils.execute(id, args)
  utils._functions[id](args)
end

-- map factory
local map = function(mode, key, cmd, opts, defaults)
  opts = vim.tbl_deep_extend("force", { silent = true }, defaults or {}, opts or {})

  if type(cmd) == "function" then
    local fn_id = utils.create(cmd)
    cmd = fmt("<CMD>lua utils.execute(%s)<CR>", fn_id)
  end

  if opts.buffer ~= nil then
    local buffer = opts.buffer
    opts.buffer = nil
    return api.nvim_buf_set_keymap(buffer, mode, key, cmd, opts)
  else
    return api.nvim_set_keymap(mode, key, cmd, opts)
  end
end

function utils.map(mode, key, cmd, opt, defaults)
  return map(mode, key, cmd, opt, defaults)
end

function utils.nmap(key, cmd, opts)
  return map("n", key, cmd, opts)
end
function utils.vmap(key, cmd, opts)
  return map("v", key, cmd, opts)
end
function utils.xmap(key, cmd, opts)
  return map("x", key, cmd, opts)
end
function utils.imap(key, cmd, opts)
  return map("i", key, cmd, opts)
end
function utils.omap(key, cmd, opts)
  return map("o", key, cmd, opts)
end
function utils.smap(key, cmd, opts)
  return map("s", key, cmd, opts)
end

function utils.nnoremap(key, cmd, opts)
  return map("n", key, cmd, opts, { noremap = true })
end
function utils.vnoremap(key, cmd, opts)
  return map("v", key, cmd, opts, { noremap = true })
end
function utils.xnoremap(key, cmd, opts)
  return map("x", key, cmd, opts, { noremap = true })
end
function utils.inoremap(key, cmd, opts)
  return map("i", key, cmd, opts, { noremap = true })
end
function utils.onoremap(key, cmd, opts)
  return map("o", key, cmd, opts, { noremap = true })
end
function utils.snoremap(key, cmd, opts)
  return map("s", key, cmd, opts, { noremap = true })
end

---@class Autocommand
---@field events string[] list of autocommand events
---@field targets string[] list of autocommand patterns
---@field modifiers string[] e.g. nested, once
---@field command string | function

-- autocommand factory
-- @param name string
-- @param commands Autocommand[]
function utils.augroup(name, autocmds, noclear)
  vim.cmd("augroup " .. name)
  if not noclear then
    vim.cmd "autocmd!"
  end
  for _, c in ipairs(autocmds) do
    local command = c.command
    if type(command) == "function" then
      local fn_id = utils.create(command)
      command = fmt("lua utils.execute(%s)", fn_id)
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

-- command factory
-- USAGE utils.command("MyCommand", my_fn)
-- @param name string
-- @param fn vim command or lua function
function utils.command(name, fn)
  if type(fn) == "function" then
    local fn_id = utils.create(fn)
    fn = fmt("lua utils.execute(%s)", fn_id)
  end
  vim.cmd(fmt("command! %s %s", name, fn))
end

return utils
