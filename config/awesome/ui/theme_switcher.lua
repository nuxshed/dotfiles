local awful = require "awful"
local beautiful = require "beautiful"
local config_dir = require("gears").filesystem.get_configuration_dir()
local helpers = require "helpers"
local wibox = require "wibox"

local function make_theme(name)
  local theme_widget = wibox.widget {
    widget = wibox.container.background,
    bg = beautiful.bg_normal,
    shape = helpers.rrect(5),
    {
      widget = wibox.container.margin,
      margins = 20,
      {
        widget = wibox.widget.textbox,
        markup = name,
      },
    },
    buttons = {
      awful.button({}, 1, function()
        awful.spawn("sed -i 's/" .. theme .. "/" .. name .. "/' " .. config_dir .. "/rc.lua")
        awful.spawn("feh --bg-scale " .. config_dir .. "/themes/" .. name .. "/background.png")
        awesome.restart()
      end),
    },
  }

  theme_widget:connect_signal("mouse::enter", function()
    theme_widget.bg = beautiful.bg_focus
  end)

  theme_widget:connect_signal("mouse::leave", function()
    theme_widget.bg = beautiful.bg_normal
  end)

  return theme_widget
end

local arctic = make_theme "arctic"
local forest = make_theme "forest"
local night = make_theme "night"

local theme_switcher = awful.popup {
  widget = {
    widget = wibox.container.margin,
    margins = 20,
    {
      layout = wibox.layout.fixed.horizontal,
      spacing = 20,
      arctic,
      forest,
      night,
    },
  },
  ontop = true,
  placement = awful.placement.centered,
  visible = false,
  bg = beautiful.bg_dark,
  shape = helpers.rrect(9),
  forced_width = 500,
  forced_height = 500,
}

local function toggle()
  theme_switcher.visible = not theme_switcher.visible
end

return toggle
