local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local wibox = require "wibox"
local M = require "ui.gooey"

local butt = M.make_button {
  width = 205,
  height = 50,
  tooltip = "Surprise!",
  tooltip_opts = {
    outside_pos = "bottom",
    mode = "mouse",
  },
  hover = true,
  text = "Hover over me!",
  margins = 9,
}

local butt2 = M.make_button {
  width = 205,
  height = 50,
  text = "DO NOT CLICK",
  hover = true,
  exec = function()
    awful.spawn "notify-send 'STOP CLICKING'"
  end,
}

local my_toggle = M.make_sliding_toggle {
  initial_state = false,
  width = 46,
  height = 23,
  thumb_size = 23,
  bg_on = "#FFFFFF",
  bg_off = "#FFFFFF",
  thumb_on_bg = "#b0a2e7",
  thumb_off_bg = "#929292",
}

local toggle_button_icon = M.make_toggle {
  initial_state = false,
  width = 60,
  height = 90,
  icon_on = "nixos",
  icon_off = "emacs",
}

local radio_group = M.make_radio_group {
  options = {
    { text = "Option 1" },
    { text = "Option 2" },
    { text = "Option 3" },
  },
  on_select = function(index, option)
    print("Selected option " .. index .. ": " .. option.text)
  end,
}

local grid1 ={
   butt,
   butt2
}

local grid2 = {
  M.make_toggle { text = "toggle", width = 100, height = 60, initial_state = true },
  M.make_toggle { text = "", font = beautiful.font_name .. "24", width = 100, height = 60 },
  radio_group,
  toggle_button_icon
}

local grid3 = {
   my_toggle
}

local grid_layout1 = M.make_grid_layout(grid1, 2, 1)
local grid_layout2 = M.make_grid_layout(grid2, 2, 2)
local grid_layout3 = M.make_grid_layout(grid3, 1, 1)

local tabs = {
  { label = "Tab 1", content = grid_layout1 },
  { label = "Tab 2", content = grid_layout2 },
  { label = "Tab 3", content = grid_layout3 },
}

local tabbed_popup = M.make_tabbed_popup(tabs, {
  visible = true,
  placement = awful.placement.top,
})

