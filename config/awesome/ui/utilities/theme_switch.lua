local awful = require "awful"
local beautiful = require "beautiful"
local config_dir = require("gears").filesystem.get_configuration_dir()
local gooey = require "ui.gooey"
local wibox = require "wibox"

F.theme_switch = {}

local function make_theme_button(name)
  return gooey.make_button {
    text = name,
    hover = true,
    width = 100,
    height = 70,
    margins = 15,
    exec = function()
      awful.spawn("sed -i 's/" .. theme .. "/" .. name .. "/' " .. config_dir .. "/rc.lua")
      awful.spawn("feh --bg-scale " .. config_dir .. "/themes/" .. name .. "/background.png")
      awesome.restart()
    end,
  }
end

local cafe = make_theme_button "cafe"
local forest = make_theme_button "forest"
local material = make_theme_button "material"

local theme_switcher = awful.popup {
  widget = {
    widget = wibox.container.margin,
    margins = 20,
    {
      layout = wibox.layout.fixed.horizontal,
      spacing = 20,
      cafe,
      forest,
      material,
    },
  },
  ontop = true,
  placement = awful.placement.centered,
  visible = false,
  bg = beautiful.bg_dark,
  forced_width = 700,
  forced_height = 300,
}

function F.theme_switch.toggle()
  theme_switcher.visible = not theme_switcher.visible
end
