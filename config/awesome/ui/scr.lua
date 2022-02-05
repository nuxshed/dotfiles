local awful = require "awful"
local beautiful = require "beautiful"
local gooey = require "ui.gooey"
local wibox = require "wibox"

F.scr = {}

local clip_enabled = false

local function scr(type)
  local clip = ""
  if clip_enabled then
    clip = "toclip"
  end
  if type == "sel" then
    awful.spawn("scr selection" .. clip)
  elseif type == "window" then
    awful.spawn("scr window" .. clip)
  else
    awful.spawn("scr screen" .. clip)
  end
end

local selection = gooey.make_button {
  icon = "crop",
  bg = beautiful.bg_subtle,
  hover = true,
  exec = function()
    F.scr.toggle()
    scr "sel"
  end,
}

local window = gooey.make_button {
  icon = "window",
  bg = beautiful.bg_subtle,
  hover = true,
  exec = function()
    F.scr.toggle()
    scr "window"
  end,
}

local screen = gooey.make_button {
  icon = "monitor",
  bg = beautiful.bg_subtle,
  hover = true,
  exec = function()
    F.scr.toggle()
    scr "screen"
  end,
}

local clip_switch = gooey.make_switch {
  icon = "clipboard",
  bg = beautiful.bg_subtle,
  bg_off = beautiful.bg_subtle,
  exec_on = function()
    clip_enabled = true
  end,
  exec_off = function()
    clip_enabled = false
  end,
  width = 40,
  height = 40,
  margins = 10,
  hover = true,
}

local scr_popup = awful.popup {
  widget = {
    {
      {
        {
          widget = wibox.widget.textbox,
          font = "Cartograph CF Medium 14",
          text = "Screenshot",
        },
        nil,
        clip_switch,
        layout = wibox.layout.align.horizontal,
      },
      {
        layout = wibox.layout.fixed.horizontal,
        spacing = 20,
        selection,
        window,
        screen,
      },
      layout = wibox.layout.fixed.vertical,
      spacing = 10,
    },
    widget = wibox.container.margin,
    margins = 20,
  },
  ontop = true,
  placement = awful.placement.centered,
  visible = false,
  border_color = beautiful.bg_subtle,
  border_width = 2,
  bg = beautiful.bg_normal,
}

function F.scr.toggle()
  scr_popup.visible = not scr_popup.visible
end
