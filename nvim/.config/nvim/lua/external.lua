local M = {
  kitty = {},
}

local fn = vim.fn
local fmt = string.format

function M.kitty.set_background(bg)
  if vim.env.KITTY_LISTEN_ON then
    fn.system(
      fmt(
        "kitty @ --to %s set-colors background=" .. bg,
        vim.env.KITTY_LISTEN_ON
      )
    )
  end
end
-- TODO: i should probavly add an autocmd to reset this

return M
