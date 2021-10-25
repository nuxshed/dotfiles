local awful = require "awful"
local bling = require "modules.bling"
local machi = require "modules.layout-machi"
local l = awful.layout.suit

machi.editor.nested_layouts = {
  ["0"] = bling.layout.deck,
  ["1"] = bling.layout.centered,
  ["3"] = bling.layout.equalarea,
  ["4"] = bling.layout.mstab,
}

require("beautiful").layout_machi = machi.get_icon()

awful.layout.layouts = {
  l.floating,
  l.tile,
  l.spiral,
  bling.layout.mstab,
  bling.layout.centered,
  bling.layout.equalarea,
  bling.layout.deck,
  machi.default_layout,
}
