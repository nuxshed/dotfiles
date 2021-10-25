local awful = require "awful"
local bling = require "modules.bling"
local l = awful.layout.suit

awful.layout.layouts = {
  l.floating,
  l.tile,
  l.spiral,
  bling.layout.mstab,
  bling.layout.centered,
  bling.layout.equalarea,
  bling.layout.deck,
}
