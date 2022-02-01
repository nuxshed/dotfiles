local awful = require "awful"
local beautiful = require "beautiful"
local icons_dir = require("gears").filesystem.get_configuration_dir() .. "/icons/"
local wibox = require "wibox"

F.exit = {}

local function make_exit_button(icon, exec)
  local exit_button = wibox.widget {
    widget = wibox.container.background,
    forced_width = 100,
    forced_height = 100,
    bg = beautiful.bg_subtle,
    {
      widget = wibox.container.margin,
      margins = 30,
      {
        widget = wibox.widget.imagebox,
        image = icons_dir .. icon .. ".svg",
        stylesheet = " * { stroke: " .. beautiful.fg_normal .. " }",
      },
    },
    buttons = {
      awful.button({}, 1, function()
        awful.spawn(exec)
      end),
    },
  }

  exit_button:connect_signal("mouse::enter", function()
    exit_button.bg = beautiful.bg_focus
  end)

  exit_button:connect_signal("mouse::leave", function()
    exit_button.bg = beautiful.bg_subtle
  end)

  return exit_button
end

local poweroff = make_exit_button("poweroff", "shutdown")
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
