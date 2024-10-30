local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local rubato = require "modules.rubato"
local wibox = require "wibox"
local gooey = require "ui.gooey"

F.action = {}

-- Helper Functions

local function make_button(opts)
  return gooey.make_button {
    icon = opts.icon,
    icon_fg = opts.icon_fg or "#00000000",
    bg = opts.bg or beautiful.bg_subtle,
    bg_hover = opts.bg_hover or beautiful.bg_focus,
    margins = opts.margins or 25,
    exec = opts.exec,
  }
end

local function make_toggle(opts)
  return gooey.make_toggle {
    initial_state = opts.initial_state or false,
    width = opts.width or 100,
    icon_on = opts.icon_on or "default_on_icon",
    icon_off = opts.icon_off or "default_off_icon",
    margins = opts.margins or 25,
    border_color = opts.border_color or beautiful.bg_focus,
    border_width = opts.border_width or 4,
    exec_on = opts.exec_on,
    exec_off = opts.exec_off,
  }
end

local browser = make_button {
  icon = "firefox",
  exec = function()
    awful.spawn.easy_async("firefox", function() end)
  end,
}
local editor = make_button {
  icon = "emacs",
  exec = function()
    awful.spawn.easy_async("emacs", function() end)
  end,
}
local notes = make_button {
  icon = "obsidian",
  exec = function()
    awful.spawn.easy_async("obsidian", function() end)
  end,
}
local inkscape = make_button {
  icon = "inkscape",
  exec = function()
    awful.spawn.easy_async("inkscape", function() end)
  end,
}

local apps = wibox.widget {
  widget = wibox.container.margin,
  left = 18,
  forced_width = 200,
  bg = "#b0a2e7",
  {
    layout = wibox.layout.fixed.horizontal,
    spacing = 15,
    browser,
    editor,
    notes,
    inkscape,
  },
}

local screenshot = make_button {
  icon = "crop",
  icon_fg = beautiful.fg_normal,
  exec = function()
    awful.spawn.easy_async("scr selectiontoclip", function() end)
  end,
}
local recording = make_button {
  icon = "video",
  icon_fg = beautiful.fg_normal,
  exec = function()
    awful.spawn.easy_async("emacs", function() end)
  end,
}
local lock_screen = make_button {
  icon = "lock",
  icon_fg = beautiful.fg_normal,
  exec = function()
    awful.spawn.easy_async("loginctl lock-session", function() end)
  end,
}
local power = make_button {
  icon = "power",
  icon_fg = beautiful.fg_normal,
  exec = function()
    awful.spawn.easy_async("notify-send 'TODO'", function() end)
  end,
}

local actions = wibox.widget {
  widget = wibox.container.margin,
  left = 18,
  forced_width = 200,
  bg = "#b0a2e7",
  {
    layout = wibox.layout.fixed.horizontal,
    spacing = 15,
    screenshot,
    recording,
    lock_screen,
    power,
  },
}

local tabbed_widget = gooey.make_tabbed_widget({
  { label = "Apps", content = apps },
  { label = "Files", content = wibox.widget.textbox "" },
  { label = "Actions", content = actions },
}, { forced_height = 110 })

local brightness_slider = wibox.widget {
  widget = wibox.widget.slider,
  bar_height = 30,
  bar_active_color = beautiful.bg_focus,
  bar_color = beautiful.bg_subtle,
  handle_color = beautiful.fg_minimize,
  handle_shape = gears.shape.square,
  handle_width = 26,
  handle_margins = { top = 16, bottom = 16 },
  value = 30,
  maximum = 100,
}

brightness_slider:connect_signal("property::value", function(_, value)
  awful.spawn("brightnessctl set " .. value .. "%", false)
end)

local volume_slider = wibox.widget {
  widget = wibox.widget.slider,
  bar_height = 30,
  bar_active_color = beautiful.bg_focus,
  bar_color = beautiful.bg_subtle,
  handle_color = beautiful.fg_minimize,
  handle_shape = gears.shape.square,
  handle_width = 26,
  handle_margins = { top = 16, bottom = 16 },
  value = 50,
  maximum = 100,
}

volume_slider:connect_signal("property::value", function(_, value)
  awful.spawn("amixer set Master " .. value .. "%", false)
end)

local section1 = wibox.widget {
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
  forced_height = 100,
  forced_width = 200,
  border_width = 4,
  border_color = beautiful.bg_focus,
  {
    widget = wibox.container.margin,
    margins = 30,
    {
      layout = wibox.layout.flex.vertical,
      spacing = 0,
      volume_slider,
      brightness_slider,
    },
  },
}

local function update_brightness_slider()
  awful.spawn.easy_async_with_shell("brightnessctl get", function(stdout)
    local brightness = tonumber(stdout) or 30
    awful.spawn.easy_async_with_shell("brightnessctl max", function(max_brightness)
      max_brightness = tonumber(max_brightness) or 100
      brightness_slider.value = (brightness / max_brightness) * 100
    end)
  end)
end

local function update_volume_slider()
  awful.spawn.easy_async_with_shell("amixer get Master", function(stdout)
    local volume = stdout:match "(%d?%d?%d)%%"
    volume = tonumber(volume) or 50
    volume_slider.value = volume
  end)
end

update_brightness_slider()
update_volume_slider()

local wifi = make_toggle {
  initial_state = true,
  icon_on = "wifi",
  icon_off = "wifi-off",
  exec_on = function()
    awful.spawn.easy_async("nmcli radio wifi on", function() end)
  end,
  exec_off = function()
    awful.spawn.easy_async("nmcli radio wifi off", function() end)
  end,
}

local bluetooth = make_toggle {
  initial_state = false,
  icon_on = "bluetooth",
  icon_off = "bluetooth",
  exec_on = function()
    awful.spawn.easy_async("bluetoothctl power on", function() end)
  end,
  exec_off = function()
    awful.spawn.easy_async("bluetoothctl power off", function() end)
  end,
}

local backlight = make_toggle {
  initial_state = true,
  icon_on = "sunrise",
  icon_off = "sunset",
}

local dnd = make_toggle {
  initial_state = false,
  icon_on = "moon",
  icon_off = "moon",
  exec_on = function()
    awful.spawn.easy_async("notify-send 'Do Not Disturb: On'", function() end)
  end,
  exec_off = function()
    awful.spawn.easy_async("notify-send 'Do Not Disturb: Off'", function() end)
  end,
}

local section2 = wibox.widget {
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
  forced_height = 80,
  forced_width = 250,
  border_width = 4,
  border_color = beautiful.bg_focus,
  {
    widget = wibox.container.margin,
    margins = 20,
    left = 24,
    {
      layout = wibox.layout.fixed.horizontal,
      spacing = 25,
      wifi,
      bluetooth,
      backlight,
      dnd,
    },
  },
}

-- why is this an arcchart?
local bat_arcchart = wibox.widget {
  widget = wibox.container.arcchart,
  start_angle = math.pi / 2,
  thickness = 8,
  value = 100,
  min_value = 0,
  max_value = 100,
  colors = { beautiful.fg_normal },
  bg = beautiful.fg_minimize,
}

local battery_percent = wibox.widget {
  widget = wibox.widget.textbox,
  font = beautiful.font_name .. " Bold 12",
  valign = "center",
  align = "center",
  text = "lol",
}

local battery = wibox.widget {
  bg = beautiful.bg_focus,
  widget = wibox.container.background,
  {
    {
      layout = wibox.layout.stack,
      battery_percent,
      bat_arcchart,
    },
    widget = wibox.container.margin,
    margins = 15,
  },
}

awesome.connect_signal("squeal::battery", function(capacity, status)
  local fill_color = beautiful.fg_normal

  if capacity >= 11 and capacity <= 35 then
    fill_color = beautiful.warn
  elseif capacity <= 10 then
    fill_color = beautiful.critical
  end

  if status == "Charging\n" then
    fill_color = beautiful.green
  end

  bat_arcchart.value = capacity
  bat_arcchart.colors = { fill_color }
  battery_percent.text = tostring(capacity)
end)

local date = wibox.widget {
  widget = wibox.container.background,
  bg = beautiful.bg_focus,
  {
    widget = wibox.container.margin,
    margins = 15,
    top = 10,
    {
      layout = wibox.layout.fixed.vertical,
      spacing = 5,
      {
        widget = wibox.widget.textclock "%a",
        font = beautiful.font_name .. " Regular 11",
        align = "center",
      },
      {
        widget = wibox.widget.textclock "%d",
        font = beautiful.font_name .. " Medium 36",
        align = "center",
      },
    },
  },
}

local time = wibox.widget {
  widget = wibox.container.background,
  forced_height = 60,
  bg = beautiful.bg_focus,
  {
    widget = wibox.container.margin,
    margins = 15,
    top = 13,
    {
      layout = wibox.layout.fixed.vertical,
      {
        widget = wibox.widget.textclock "%H",
        font = beautiful.font_name .. " Medium 18",
        align = "center",
      },
      {
        widget = wibox.widget.textclock "%M",
        font = beautiful.font_name .. " Medium 18",
        align = "center",
      },
    },
  },
}

-- tile, spiral, tilebottom, mstab, centered, equalarea, deck, floating
local layouts = {
  ["tile"] = "<t>",
  ["spiral"] = "<s>",
  ["tilebottom"] = "<b>",
  ["mstab"] = "<tb>",
  ["centered"] = "<c>",
  ["equalarea"] = "<eq>",
  ["deck"] = "<d>",
  ["floating"] = "<f>",
}

local function lname()
  local current_layout = awful.layout.getname(s)
  for key, value in pairs(layouts) do
    if current_layout == key then
      return value
    end
  end
  return ""
end

local layoutname = wibox.widget {
  widget = wibox.widget.textbox,
  text = lname(),
  halign = "center",
  font = beautiful.font_name .. "Regular 12",
}

local layoutbox = wibox.widget {
  widget = wibox.container.background,
  forced_height = 60,
  bg = beautiful.bg_focus,
  buttons = {
    awful.button({}, 1, function()
      awful.layout.inc(1)
      layoutname:set_text(lname())
    end),
    awful.button({}, 3, function()
      awful.layout.inc(-1)
      layoutname:set_text(lname())
    end),
  },
  {
    layout = wibox.layout.fixed.vertical,
    spacing = 0,
    {
      widget = wibox.container.margin,
      margins = 15,
      left = 20,
      right = 20,
      bottom = 7.5,
      {
        widget = awful.widget.layoutbox {
          screen = s,
        },
      },
    },
    layoutname,
  },
}

local section3 = wibox.widget {
  widget = wibox.container.background,
  bg = beautiful.bg_normal,
  border_width = 4,
  border_color = beautiful.bg_focus,
  forced_height = 80,
  forced_width = 200,
  {
    widget = wibox.container.margin,
    margins = 20,
    {
      layout = wibox.layout.fixed.horizontal,
      spacing = 18,
      battery,
      date,
      time,
      layoutbox,
    },
  },
}

local grid_layout = gooey.make_grid_layout({
  tabbed_widget,
  section1,
  section2,
  section3,
}, 2, 2, { spacing = 20 })

local action = awful.popup {
  widget = {
    widget = wibox.container.margin,
    margins = 30,
    forced_width = 1000,
    forced_height = 400,
    grid_layout,
  },
  placement = function(c)
    awful.placement.bottom(c, { margins = { bottom = 0 } })
  end,
  ontop = true,
  visible = false,
  bg = beautiful.bg_dark,
  border_color = beautiful.bg_focus,
  border_width = 8,
}

local slide = rubato.timed {
  pos = 1600,
  rate = 60,
  intro = 0.3,
  duration = 0.8,
  easing = rubato.quadratic,
  awestore_compat = true,
  subscribed = function(pos)
    action.y = pos
  end,
}

local action_status = false

slide.ended:subscribe(function()
  if action_status then
    action.visible = false
  end
end)

local function action_show()
  action.visible = true
  slide:set(1184)
  action_status = false
end

local function action_hide()
  slide:set(1600)
  action_status = true
end

F.action.toggle = function()
  if action.visible then
    action_hide()
  else
    action_show()
  end
end
