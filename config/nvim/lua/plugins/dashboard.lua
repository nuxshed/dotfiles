local a = require "alpha"
local d = require "alpha.themes.dashboard"
local b = d.button
local s = d.section

s.buttons.val = {
  b("SPC f f", "  Find File"),
  b("SPC f r", "  Recent Files"),
  b("SPC f w", "  Find Word"),
  b("SPC f p", "  Projects"),
  b("SPC ,", "  Edit Config"),
}

s.footer.val = vim.fn.system "fortune -s"

local opts = {
  layout = {
    { type = "padding", val = 1 },
    s.header,
    { type = "padding", val = 2 },
    s.buttons,
    { type = "padding", val = 1 },
    s.footer,
  },
  opts = {
    margin = 10,
  },
}

a.setup(opts)
