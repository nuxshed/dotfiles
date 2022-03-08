local awful = require "awful"
local beautiful = require "beautiful"
local wibox = require "wibox"
local gooey = require "ui.gooey"

F.start = {}

local app_factory = function(icon, exec)
  return gooey.make_button {
    icon = icon,
    bg = beautiful.bg_subtle,
    hover = true,
    width = 50,
    height = 50,
    margins = 12,
    exec = function()
      awful.spawn(exec)
    end,
  }
end

local app_browser = app_factory("browser", "brave")
local app_terminal = app_factory("term", "kitty")
local app_emacs = app_factory("emacs", "emacs")
local app_zathura = app_factory("zathura", "zathura")
local app_gimp = app_factory("gimp", "gimp")
local app_inkscape = app_factory("inkscape", "inkscape")

local apps = wibox.widget {
  layout = wibox.layout.grid,
  spacing = 5,
  forced_num_cols = 3,
  forced_num_rows = 2,
  app_browser,
  app_terminal,
  app_gimp,
  app_emacs,
  app_zathura,
  app_inkscape,
}

local battery_circle = wibox.widget {
  value = 0,
  border_width = 6,
  widget = wibox.container.radialprogressbar,
  color = beautiful.fg_normal,
  border_color = beautiful.bg_focus,
  {
    {
      widget = wibox.widget.imagebox,
      image = beautiful.charge_icon,
      stylesheet = " * { stroke: " .. beautiful.fg_normal .. " }",
      valign = "center",
      halign = "center",
    },
    widget = wibox.container.margin,
    margins = 20,
  },
}

awesome.connect_signal("squeal::battery", function(capacity, status)
  local fill_color = beautiful.fg_normal

  if capacity >= 11 and capacity <= 30 then
    fill_color = beautiful.warn
  elseif capacity <= 10 then
    fill_color = beautiful.critical
  end

  if status == "Charging\n" then
    fill_color = beautiful.green
  end

  battery_circle.value = capacity / 100
  battery_circle.color = fill_color
end)

local battery = wibox.widget {
  widget = wibox.container.background,
  bg = beautiful.bg_subtle,
  forced_width = 100,
  forced_height = 100,
  {
    widget = wibox.container.margin,
    margins = 15,
    battery_circle,
  },
}

local start = awful.popup {
  widget = {
    {
      {
        apps,
        battery,
        layout = wibox.layout.fixed.horizontal,
        spacing = 30,
      },
      layout = wibox.layout.fixed.vertical,
      spacing = 30,
    },
    widget = wibox.container.margin,
    margins = 30,
  },
  ontop = true,
  placement = function(c)
    (awful.placement.top_left)(c, { margins = { left = 65, top = 10 } })
  end,
  visible = false,
  border_color = beautiful.bg_subtle,
  border_width = 2,
  bg = beautiful.bg_normal,
}

function F.start.toggle()
  start.visible = not start.visible
end
