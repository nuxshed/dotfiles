--   ▄████  ▒█████   ▒█████  ▓█████▓██   ██▓
--  ██▒ ▀█▒▒██▒  ██▒▒██▒  ██▒▓█   ▀ ▒██  ██▒
-- ▒██░▄▄▄░▒██░  ██▒▒██░  ██▒▒███    ▒██ ██░
-- ░▓█  ██▓▒██   ██░▒██   ██░▒▓█  ▄  ░ ▐██▓░
-- ░▒▓███▀▒░ ████▓▒░░ ████▓▒░░▒████▒ ░ ██▒▓░
--  ░▒   ▒ ░ ▒░▒░▒░ ░ ▒░▒░▒░ ░░ ▒░ ░  ██▒▒▒
--   ░   ░   ░ ▒ ▒░   ░ ▒ ▒░  ░ ░  ░▓██ ░▒░
-- ░ ░   ░ ░ ░ ░ ▒  ░ ░ ░ ▒     ░   ▒ ▒ ░░
--       ░     ░ ░      ░ ░     ░  ░░ ░
--                                  ░ ░
-- 0.0 (nuxsh)

-- [TODO]
-- checkboxes
-- chips?
-- carousels?
-- dropdown select?
-- text input (TRIED -> FAILED)
-- remember toggle states (HARD)
-- accordions?
-- sliders? (already pretty high level in awesome)

local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local wibox = require "wibox"

local icons_dir = gears.filesystem.get_configuration_dir() .. "/icons/"

local M = {}

-- Helper function to create an icon widget
function M.create_icon_widget(icon, icon_color)
  return wibox.widget {
    widget = wibox.widget.imagebox,
    halign = "center",
    image = icons_dir .. icon .. ".svg",
    color = icon_color or beautiful.fg_normal,
  }
end

-- Helper function to create a text widget
function M.create_text_widget(text, font)
  return wibox.widget {
    widget = wibox.widget.textbox,
    align = "center",
    valign = "center",
    markup = text,
    font = font or beautiful.font,
  }
end

-- Helper function to add a tooltip to a widget
function M.add_tooltip(widget, text, opts)
  opts = opts or {}
  local tooltip = awful.tooltip {
    objects = { widget },
    text = text,
    mode = opts.mode or "outside",
    preferred_positions = opts.outside_pos or "left",
    preferred_alignments = opts.outside_align or "middle",
    margins = opts.margins or 8,
  }
end

-- Create a button widget
function M.make_button(opts)
  opts = opts or {}

  local icon_widget = opts.icon and M.create_icon_widget(opts.icon, opts.icon_fg or beautiful.fg_normal) or nil
  local text_widget = M.create_text_widget(opts.text or "Button", opts.font or beautiful.font)

  local inner_widget = icon_widget or text_widget

  local button = wibox.widget {
    widget = wibox.container.background,
    forced_width = opts.width or 100,
    forced_height = opts.height or 100,
    bg = opts.bg or beautiful.bg_normal,
    fg = opts.fg or beautiful.fg_normal,
    {
      widget = wibox.container.place,
      halign = "center",
      valign = "center",
      {
        widget = wibox.container.margin,
        margins = opts.margins or 0,
        inner_widget,
      },
    },
    buttons = {
      awful.button({}, 1, function()
        if opts.exec then
          opts.exec()
        end
      end),
    },
  }

  if opts.bg_hover then
    button:connect_signal("mouse::enter", function()
      button.bg = opts.bg_hover or beautiful.bg_focus
    end)

    button:connect_signal("mouse::leave", function()
      button.bg = opts.bg or beautiful.bg_normal
    end)
  end

  if opts.tooltip then
    M.add_tooltip(button, opts.tooltip, opts.tooltip_opts or nil)
  end

  return button
end

-- Create a radio button widget
function M.make_radio_button(opts)
  opts = opts or {}

  local icon_widget = opts.icon and M.create_icon_widget(opts.icon, opts.icon_fg or beautiful.fg_normal) or nil
  local text_widget = opts.text and M.create_text_widget(opts.text or "Radio", opts.font or beautiful.font) or nil

  local inner_widget = icon_widget or text_widget

  local button = wibox.widget {
    widget = wibox.container.background,
    forced_width = opts.width or 100,
    forced_height = opts.height or 30,
    bg = opts.bg_off or beautiful.bg_normal,
    fg = opts.fg_off or beautiful.fg_minimize,
    {
      widget = wibox.container.margin,
      margins = opts.margins or 10,
      inner_widget,
    },
    buttons = {
      awful.button({}, 1, function()
        if opts.exec then
          opts.exec()
        end
      end),
    },
  }

  if opts.tooltip then
    M.add_tooltip(button, opts.tooltip)
  end

  return button
end

-- Create a radio group wiobsidiandget
function M.make_radio_group(opts)
  opts = opts or {}
  local buttons = {}
  local group_widget = wibox.widget {
    layout = wibox.layout.fixed.vertical,
  }

  local function update_buttons(selected_index)
    for i, button in ipairs(buttons) do
      if i == selected_index then
        button.bg = opts.bg_on or beautiful.bg_focus
        button.fg = opts.fg_on or beautiful.fg_focus
      else
        button.bg = opts.bg_off or beautiful.bg_normal
        button.fg = opts.fg_off or beautiful.fg_minimize
      end
    end
  end

  for i, option in ipairs(opts.options) do
    local button = M.make_radio_button {
      text = option.text,
      icon = option.icon,
      bg_off = opts.bg_off,
      fg_off = opts.fg_off,
      bg_on = opts.bg_on,
      fg_on = opts.fg_on,
      exec = function()
        update_buttons(i)
        if opts.on_select then
          opts.on_select(i, option)
        end
      end,
    }
    table.insert(buttons, button)
    group_widget:add(button)
  end

  return group_widget
end

-- Create a prompt widget
function M.make_prompt_widget(prompt, opts)
  opts = opts or {}
  return awful.popup {
    widget = {
      widget = wibox.container.margin,
      margins = opts.margins or 20,
      {
        layout = wibox.layout.fixed.horizontal,
        spacing = 10,
        {
          widget = wibox.container.background,
          bg = opts.bg or beautiful.bg_focus,
          {
            widget = wibox.widget.textbox,
            font = beautiful.font_name .. "Regular 16",
            text = opts.mode or "",
          },
        },
        prompt,
      },
    },
    ontop = true,
    placement = opts.placement or awful.placement.centered,
    visible = false,
    border_color = opts.border_color or beautiful.border_color_active,
    border_width = opts.border_width or 4,
    bg = opts.bg or beautiful.bg_normal,
    forced_width = opts.forced_width or 500,
    forced_height = opts.forced_height or 500,
  }
end

-- Create a toggle widget
function M.make_toggle(opts)
  opts = opts or {}

  local state = opts.initial_state or false
  local icon_widget = wibox.widget {
    widget = wibox.widget.imagebox,
    halign = "center",
    image = icons_dir .. opts.icon .. ".svg",
    stylesheet = " * { stroke: " .. (state and opts.icon_fg_on or opts.icon_fg_off) .. " }",
  }

  local button = wibox.widget {
    {
      icon_widget,
      margins = opts.margins or 25,
      widget = wibox.container.margin,
    },
    forced_width = opts.width or 100,
    forced_height = opts.height or 100,
    bg = state and (opts.bg_on or beautiful.bg_focus) or (opts.bg_off or beautiful.bg_normal),
    widget = wibox.container.background,
  }

  local function update_state()
    button.bg = state and (opts.bg_on or beautiful.bg_focus) or (opts.bg_off or beautiful.bg_normal)
    icon_widget.stylesheet = " * { stroke: " .. (state and opts.icon_fg_on or opts.icon_fg_off) .. " }"
  end

  update_state()

  button:connect_signal("mouse::enter", function()
    if not state then
      button.bg = opts.bg_hover or beautiful.bg_focus
    end
  end)

  button:connect_signal("mouse::leave", function()
    if not state then
      button.bg = opts.bg_off or beautiful.bg_normal
    end
  end)

  button:buttons(gears.table.join(
    awful.button({}, 1, function()
      state = not state
      update_state()
      if opts.exec then opts.exec(state) end
    end)
  ))

  return button
end

-- Create a checkbox
function M.make_checkbox(opts)
  opts = opts or {}

  opts.bg_off = opts.bg_off or beautiful.bg_normal
  opts.border_width = opts.border_width or 3

  return M.make_toggle(opts)
end

-- Create a sliding toggle switch
-- FIXME: weird at startup sometimes
function M.make_sliding_toggle(opts)
  opts = opts or {}
  local state = opts.initial_state or false
  local toggle_thumb_size = opts.thumb_size or 30
  local total_width = opts.width or 60
  local toggle_height = opts.height or 30
  local thumb_margins = (toggle_height - toggle_thumb_size) / 2

  local toggle_thumb = wibox.widget {
    widget = wibox.container.background,
    shape = gears.shape.thumb,
    forced_width = toggle_thumb_size,
    forced_height = toggle_thumb_size,
    bg = state and (opts.thumb_on_bg or beautiful.bg_focus) or (opts.thumb_off_bg or beautiful.bg_minimize),
  }

  local toggle_container = wibox.widget {
    toggle_thumb,
    widget = wibox.container.margin,
    left = state and (total_width - toggle_thumb_size) or 0,
    right = state and 0 or (total_width - toggle_thumb_size),
    margins = thumb_margins,
  }

  local toggle = wibox.widget {
    {
      toggle_container,
      widget = wibox.container.background,
    },
    widget = wibox.container.background,
    forced_width = total_width,
    forced_height = toggle_height,
    shape = opts.shape or gears.shape.rectangle,
    bg = state and (opts.bg_on or beautiful.bg_focus) or (opts.bg_off or beautiful.bg_minimize),
  }

  local function update_toggle()
    toggle_thumb.bg = state and (opts.thumb_on_bg or beautiful.bg_focus) or (opts.thumb_off_bg or beautiful.bg_minimize)
    toggle_container.left = state and (total_width - toggle_thumb_size) or 0
    toggle_container.right = state and 0 or (total_width - toggle_thumb_size)
    toggle.bg = state and (opts.bg_on or beautiful.bg_focus) or (opts.bg_off or beautiful.bg_minimize)
    toggle:emit_signal "widget::redraw_needed"
  end

  toggle:connect_signal("button::press", function(_, _, _, button)
    if button == 1 then
      state = not state
      update_toggle()
      if state and opts.exec_on then
        opts.exec_on()
      elseif not state and opts.exec_off then
        opts.exec_off()
      end
    end
  end)

  return toggle
end

-- Custom grid layout
function M.make_grid_layout(widgets, rows, cols, opts)
  opts = opts or {}
  local grid = wibox.widget {
    layout = wibox.layout.grid,
    spacing = opts.spacing or 10,
    homogeneous = false,
    expand = true,
    forced_num_rows = rows,
    forced_num_cols = cols,
  }

  for _, widget in ipairs(widgets) do
    grid:add(widget)
  end

  return grid
end

-- Create a tabbed widget
function M.make_tabbed_widget(tabs, opts)
  opts = opts or {}
  local tab_buttons = {}
  local tab_content = wibox.widget {
    layout = wibox.layout.stack,
  }

  local function update_tab_highlight(active_tab_index)
    for i, button in ipairs(tab_buttons) do
      if i == active_tab_index then
        button.bg = opts.active_tab_bg or beautiful.bg_focus
      else
        button.bg = opts.inactive_tab_bg or beautiful.bg_normal
      end
    end
  end

  for i, tab in ipairs(tabs) do
    local tab_button = wibox.widget {
      widget = wibox.container.background,
      bg = (i == 1) and (opts.active_tab_bg or beautiful.bg_focus) or (opts.inactive_tab_bg or beautiful.bg_normal),
      {
        widget = wibox.container.margin,
        margins = 9,
        {
          widget = wibox.widget.textbox,
          text = tab.label,
          align = "center",
          valign = "center",
          font = opts.font or beautiful.font,
        },
      },
      buttons = {
        awful.button({}, 1, function()
          for j, content in ipairs(tab_content.children) do
            content.visible = (i == j)
          end
          update_tab_highlight(i)
        end),
      },
    }

    table.insert(tab_buttons, tab_button)

    local content_widget = wibox.widget {
      layout = wibox.container.background,
      bg = opts.content_bg or "#00000000",
      visible = (i == 1),
      {
        tab.content,
        widget = wibox.container.margin,
        margins = 10,
      },
    }

    tab_content:add(content_widget)
  end

  local tab_bar = wibox.widget {
    {
      layout = wibox.layout.flex.horizontal,
      spacing = 10,
      table.unpack(tab_buttons),
    },
    bg = opts.inactive_tab_bg or beautiful.bg_normal,
    border_width = opts.bar_border_width or 0,
    border_color = opts.bar_border_color or beautiful.bg_focus,
    widget = wibox.container.background,
  }

  local tabbed_widget = wibox.widget {
    layout = wibox.layout.fixed.vertical,
    spacing = 18,
    widget = wibox.container.background,
    forced_height = opts.forced_height or nil,
    {
      layout = wibox.layout.fixed.vertical,
      spacing = 18,
      tab_bar,
      tab_content,
    },
  }

  return tabbed_widget
end

return M
