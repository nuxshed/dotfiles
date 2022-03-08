local awful = require "awful"
local beautiful = require "beautiful"
local icons_dir = require("gears").filesystem.get_configuration_dir() .. "/icons/"
local wibox = require "wibox"

local M = {}

function M.make_button(opts)
  opts = opts or {}

  local icon = opts.icon or "default"
  local icon_color = opts.icon_fg or beautiful.fg_normal
  local icon_widget = wibox.widget {
    widget = wibox.widget.imagebox,
    image = icons_dir .. icon .. ".svg",
    -- stylesheet = " * { stroke: " .. icon_color .. " }",
  }

  local text_widget = wibox.widget {
    widget = wibox.widget.textbox,
    align = "center",
    valign = "center",
    markup = opts.text or "Button",
    font = opts.font or beautiful.font,
  }

  local inner_widget = text_widget

  if opts.icon then
    inner_widget = icon_widget
  end

  local button = wibox.widget {
    widget = wibox.container.background,
    forced_width = opts.width or 100,
    forced_height = opts.height or 100,
    bg = opts.bg or beautiful.bg_normal,
    fg = opts.fg or beautiful.fg_normal,
    {
      widget = wibox.container.margin,
      margins = opts.margins or 30,
      inner_widget,
    },
    buttons = {
      awful.button({}, 1, function()
        opts.exec()
      end),
    },
  }

  if opts.hover then
    button:connect_signal("mouse::enter", function()
      button.bg = opts.bg_hover or beautiful.bg_focus
    end)

    button:connect_signal("mouse::leave", function()
      button.bg = opts.bg or beautiful.bg_normal
    end)
  end

  return button
end

function M.make_switch(opts)
  opts = opts or {}

  local icon = opts.icon or "default"
  local icon_color = opts.icon_fg or beautiful.fg_normal
  local icon_widget = wibox.widget {
    widget = wibox.widget.imagebox,
    image = icons_dir .. icon .. ".svg",
    stylesheet = " * { stroke: " .. icon_color .. " }",
  }

  local text_widget = wibox.widget {
    widget = wibox.widget.textbox,
    markup = opts.text or "Button",
    font = opts.font or beautiful.font,
  }

  local inner_widget = text_widget

  if opts.icon then
    inner_widget = icon_widget
  end

  local button = wibox.widget {
    widget = wibox.container.background,
    forced_width = opts.width or 100,
    forced_height = opts.height or 100,
    bg = opts.bg or beautiful.bg_normal,
    fg = opts.fg or beautiful.fg_normal,
    {
      widget = wibox.container.margin,
      margins = opts.margins or 30,
      inner_widget,
    },
  }

  local s = true
  button:buttons {
    awful.button({}, 1, function()
      s = not s
      if s then
        button.bg = opts.bg_off or beautiful.bg_normal
        opts.exec_off()
      else
        button.bg = opts.bg_on or beautiful.bg_focus
        opts.exec_on()
      end
    end),
  }

  return button
end

function M.make_prompt_widget(prompt, opts)
  opts = opts or {}
  return awful.popup {
    widget = {
      widget = wibox.container.margin,
      margins = opts.margins or 20,
      prompt,
    },
    ontop = true,
    placement = opts.placement or awful.placement.centered,
    visible = false,
    border_color = opts.border_color or beautiful.border_color_active,
    border_width = opts.border_width or 2,
    bg = opts.bg or beautiful.bg_normal,
    forced_width = opts.forced_width or 500,
    forced_height = opts.forced_height or 500,
  }
end

return M
