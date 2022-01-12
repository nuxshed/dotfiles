local awful = require "awful"
local beautiful = require "beautiful"
local bling = require "modules.bling"
local l = awful.layout.suit
local machi = require "modules.layout-machi"

beautiful.layout_machi = machi.get_icon()

machi.editor.nested_layouts = {
  ["0"] = bling.layout.deck,
  ["1"] = awful.layout.suit.spiral,
  ["2"] = awful.layout.suit.fair,
  ["3"] = awful.layout.suit.fair.horizontal,
}

awful.layout.layouts = {
  l.floating,
  l.tile,
  l.spiral,
  bling.layout.mstab,
  bling.layout.centered,
  bling.layout.equalarea,
  bling.layout.deck,
  bling.layout.horizontal,
  machi.default_layout,
}
