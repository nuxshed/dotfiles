--  ____ ____ ____ ____ ____ ____ ____ ____ ____ ____
-- ||k |||e |||y |||s |||e |||q |||. |||l |||u |||a ||
-- ||__|||__|||__|||__|||__|||__|||__|||__|||__|||__||
-- |/__\|/__\|/__\|/__\|/__\|/__\|/__\|/__\|/__\|/__\|

local awful = require "awful"
local wibox = require "wibox"
local gears = require "gears"
local beautiful = require "beautiful"

local K = {}

K.timeout = 1

local sequence_display = wibox {
  ontop = true,
  visible = false,
  bg = beautiful.bg_normal,
  fg = beautiful.fg_normal,
  border_width = 4,
  border_color = beautiful.bg_focus,
  width = 180,
  height = 60,
}

local key_container = wibox.layout.fixed.horizontal()

sequence_display:setup {
  key_container,
  layout = wibox.layout.align.horizontal,
}

local function create_key_widget(key, dim)
  return wibox.widget {
    {
      {
        text = key,
        align = "center",
        valign = "center",
        font = beautiful.font .. " 14",
        widget = wibox.widget.textbox,
      },
      bg = dim and beautiful.bg_minimize or beautiful.bg_focus,
      shape = gears.shape.rectangle,
      forced_width = 40,
      forced_height = 40,
      widget = wibox.container.background,
      opacity = dim and 0.5 or 1,
    },
    margins = 10,
    widget = wibox.container.margin,
  }
end

local function update_wibox_size()
  local total_widgets = #key_container.children
  local new_width = math.max(180, total_widgets * 60)
  sequence_display.width = new_width
end

local function show_sequence()
  sequence_display.visible = true
  awful.placement.top_left(sequence_display, { margins = { top = 20, left = 20 }, parent = awful.screen.primary })
end

local function hide_sequence()
  key_container:reset()
  sequence_display.visible = false
end

local function show_message(msg)
  key_container:reset()
  local message_widget = wibox.widget {
    text = msg,
    align = "center",
    valign = "center",
    font = beautiful.font_name .. "Regular 14",
    widget = wibox.widget.textbox,
  }
  key_container:add(message_widget)
  update_wibox_size()
  show_sequence()
end

local function show_preview(sequence, bindings)
  local possible_sequences = {}
  for key_combination, _ in pairs(bindings) do
    if key_combination:sub(1, #sequence) == sequence then
      table.insert(possible_sequences, key_combination)
    end
  end

  if #possible_sequences == 1 then
    local remaining_keys = possible_sequences[1]:sub(#sequence + 2)
    for key in remaining_keys:gmatch "%S+" do
      key_container:add(create_key_widget(key, true))
    end
    update_wibox_size()
  end
end

function K.start(bindings)
  local sequence = {}
  local timer

  local function stop_grabber(grabber)
    hide_sequence()
    if timer then
      timer:stop()
    end
    grabber:stop()
  end

  local function restart_timer(grabber)
    if timer then
      timer:stop()
    end
    timer = gears.timer {
      timeout = K.timeout,
      single_shot = true,
      callback = function()
        stop_grabber(grabber)
      end,
    }
    timer:start()
  end

  local function add_to_sequence(key)
    table.insert(sequence, key)
  end

  local function update_display()
    key_container:reset()
    for _, key in ipairs(sequence) do
      key_container:add(create_key_widget(key))
    end
    update_wibox_size()
    show_sequence()
  end

  local grabber = awful.keygrabber {
    keybindings = {},
    stop_key = "Escape",
    stop_event = "release",
    start_callback = function()
      show_sequence()
    end,
    stop_callback = stop_grabber,
    keypressed_callback = function(self, _, key, _)
      add_to_sequence(key)
      update_display()
      restart_timer(self)

      local seq_str = table.concat(sequence, " ")
      if bindings[seq_str] then
        if type(bindings[seq_str]) == "function" then
          bindings[seq_str]()
          gears.timer.start_new(K.timeout, function()
            stop_grabber(self)
          end)
          return
        end
      else
        local partial = false
        for k, _ in pairs(bindings) do
          if k:sub(1, #seq_str) == seq_str then
            partial = true
            break
          end
        end
        if not partial then
          show_message " No match"
          sequence = {}
          restart_timer(self)
          return
        end
      end

      -- show_preview(seq_str, bindings)
    end,
  }

  grabber:start()
end

return K
