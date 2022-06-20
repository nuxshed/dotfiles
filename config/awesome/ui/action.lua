local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local naughty = require "naughty"
local rubato = require "modules.rubato"
local wibox = require "wibox"

F.action = {}

local notifs_text = wibox.widget {
  font = beautiful.font_name .. " Bold 9",
  markup = "Notifications",
  halign = "center",
  widget = wibox.widget.textbox,
}

local notifs_clear = wibox.widget {
  markup = "<span foreground='" .. beautiful.fg_minimize .. "'>x</span>",
  font = beautiful.font_name .. " Bold 9",
  align = "center",
  valign = "center",
  widget = wibox.widget.textbox,
}

notifs_clear:buttons(gears.table.join(awful.button({}, 1, function()
  _G.notif_center_reset_notifs_container()
end)))

local notifs_empty = wibox.widget {
  {
    nil,
    {
      nil,
      {
        markup = "<span foreground='" .. beautiful.fg_minimize .. "'>No Notifications</span>",
        align = "center",
        valign = "center",
        widget = wibox.widget.textbox,
      },
      layout = wibox.layout.align.vertical,
    },
    layout = wibox.layout.align.horizontal,
  },
  forced_height = 160,
  widget = wibox.container.background,
}

local notifs_container = wibox.widget {
  spacing = 10,
  spacing_widget = {
    {
      shape = gears.shape.rounded_rect,
      widget = wibox.container.background,
    },
    top = 2,
    bottom = 2,
    left = 6,
    right = 6,
    widget = wibox.container.margin,
  },
  forced_width = beautiful.notifs_width or 240,
  forced_height = 430,
  layout = wibox.layout.fixed.vertical,
}

local remove_notifs_empty = true

notif_center_reset_notifs_container = function()
  notifs_container:reset(notifs_container)
  notifs_container:insert(1, notifs_empty)
  remove_notifs_empty = true
end

notif_center_remove_notif = function(box)
  notifs_container:remove_widgets(box)

  if #notifs_container.children == 0 then
    notifs_container:insert(1, notifs_empty)
    remove_notifs_empty = true
  end
end

local create_notif = function(icon, n, width)
  local time = os.date "%H:%M"
  local box = {}

  box = wibox.widget {
    {
      {
        {
          {
            image = icon,
            resize = true,
            clip_shape = function(cr, width, height)
              gears.shape.rounded_rect(cr, width, height, 2)
            end,
            halign = "center",
            valign = "center",
            widget = wibox.widget.imagebox,
          },
          strategy = "exact",
          height = 50,
          width = 50,
          widget = wibox.container.constraint,
        },
        {
          {
            nil,
            {
              {
                {
                  step_function = wibox.container.scroll.step_functions.waiting_nonlinear_back_and_forth,
                  speed = 50,
                  {
                    markup = n.title,
                    font = beautiful.font_name .. " Bold 9",
                    align = "left",
                    widget = wibox.widget.textbox,
                  },
                  forced_width = 140,
                  widget = wibox.container.scroll.horizontal,
                },
                nil,
                {
                  markup = "<span foreground='" .. beautiful.fg_minimize .. "'>" .. time .. "</span>",
                  align = "right",
                  valign = "bottom",
                  font = beautiful.font,
                  widget = wibox.widget.textbox,
                },
                expand = "none",
                layout = wibox.layout.align.horizontal,
              },
              {
                markup = n.message,
                align = "left",
                forced_width = 165,
                widget = wibox.widget.textbox,
              },
              spacing = 3,
              layout = wibox.layout.fixed.vertical,
            },
            expand = "none",
            layout = wibox.layout.align.vertical,
          },
          left = 10,
          widget = wibox.container.margin,
        },
        layout = wibox.layout.align.horizontal,
      },
      margins = 15,
      widget = wibox.container.margin,
    },
    forced_height = 85,
    widget = wibox.container.background,
    bg = beautiful.bg_subtle,
  }

  box:buttons(gears.table.join(awful.button({}, 1, function()
    _G.notif_center_remove_notif(box)
  end)))

  return box
end

notifs_container:buttons(gears.table.join(
  awful.button({}, 4, nil, function()
    if #notifs_container.children == 1 then
      return
    end
    notifs_container:insert(1, notifs_container.children[#notifs_container.children])
    notifs_container:remove(#notifs_container.children)
  end),

  awful.button({}, 5, nil, function()
    if #notifs_container.children == 1 then
      return
    end
    notifs_container:insert(#notifs_container.children + 1, notifs_container.children[1])
    notifs_container:remove(1)
  end)
))

notifs_container:insert(1, notifs_empty)

naughty.connect_signal("request::display", function(n)
  if #notifs_container.children == 1 and remove_notifs_empty then
    notifs_container:reset(notifs_container)
    remove_notifs_empty = false
  end

  local appicon = n.icon or n.app_icon
  if not appicon then
    appicon = beautiful.notification_icon
  end

  notifs_container:insert(1, create_notif(appicon, n, width))
end)

local notifs = wibox.widget {
  {
    {
      nil,
      notifs_text,
      notifs_clear,
      expand = "none",
      layout = wibox.layout.align.horizontal,
    },
    left = 5,
    right = 5,
    layout = wibox.container.margin,
  },
  notifs_container,
  spacing = 20,
  layout = wibox.layout.fixed.vertical,
}

local actions = wibox.widget {
  {
    {
      {
        {
          widget = require "ui.widget.vol_slider",
        },
        {
          widget = require "ui.widget.bri_slider",
        },
        layout = wibox.layout.flex.vertical,
        spacing = 10,
      },
      {
        { widget = require "ui.controls.wifi" },
        { widget = require "ui.controls.bluetooth" },
        { widget = require "ui.controls.dnd" },
        { widget = require "ui.controls.night_light" },
        layout = wibox.layout.flex.horizontal,
        spacing = 15,
      },
      layout = wibox.layout.flex.vertical,
      spacing = 30,
    },
    widget = wibox.container.margin,
    top = 20,
    bottom = 20,
    left = 35,
    right = 35,
  },
  widget = wibox.container.background,
  bg = beautiful.bg_subtle,
}

local action = awful.popup {
  widget = {
    widget = wibox.container.margin,
    margins = 30,
    forced_width = 355,
    forced_height = 690,
    {
      layout = wibox.layout.fixed.vertical,
      notifs,
      actions,
    },
  },
  placement = function(c)
    (awful.placement.right)(c, { margins = { bottom = 50, right = 13 } })
  end,
  ontop = true,
  visible = false,
  bg = beautiful.bg_normal,
  border_color = beautiful.bg_subtle,
  border_width = 2,
}

local slide = rubato.timed {
  pos = 1500,
  rate = 60,
  intro = 0.3,
  duration = 0.8,
  easing = rubato.quadratic,
  awestore_compat = true,
  subscribed = function(pos)
    action.x = pos
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
  slide:set(995)
  action_status = false
end

local function action_hide()
  slide:set(1500)
  action_status = true
end

F.action.toggle = function()
  if action.visible then
    action_hide()
  else
    action_show()
  end
end
