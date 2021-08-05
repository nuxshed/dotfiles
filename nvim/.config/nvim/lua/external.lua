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

function M.kitty.reset_background()
  if vim.env.KITTY_LISTEN_ON then
    fn.system(
      fmt("kitty @ --to %s set-colors --reset", vim.env.KITTY_LISTEN_ON)
    )
  end
end

return M
