local awful = require "awful"
local beautiful = require "beautiful"
local gooey = require "ui.gooey"
local wibox = require "wibox"

F.exit = {}

local function make_exit_button(icon, exec)
  return gooey.make_button {
    icon = icon,
    icon_fg = beautiful.fg_normal,
    bg = beautiful.bg_subtle,
    hover = true,
    exec = function()
      awful.spawn(exec)
    end,
  }
end

local poweroff = make_exit_button("poweroff", "poweroff")
local reboot = make_exit_button("restart", "reboot")
local quit = make_exit_button("quit", "awesome-client 'awesome.quit()'")

local exit_popup = awful.popup {
  widget = {
    widget = wibox.container.margin,
    margins = 20,
    {
      layout = wibox.layout.fixed.horizontal,
      spacing = 20,
      poweroff,
      reboot,
      quit,
    },
  },
  ontop = true,
  placement = awful.placement.centered,
  visible = false,
  border_color = beautiful.bg_focus,
  border_width = 2,
  bg = beautiful.bg_normal,
  forced_width = 500,
  forced_height = 500,
}

function F.exit.toggle()
  exit_popup.visible = not exit_popup.visible
end
