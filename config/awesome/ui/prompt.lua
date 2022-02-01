local awful = require "awful"
local beautiful = require "beautiful"
local wibox = require "wibox"

F.prompt = {}

local prompt = awful.widget.prompt {
  prompt = "exec: ",
  exe_callback = awful.util.eval,
  done_callback = function()
    F.prompt.close()
  end,
}

local prompt_widget = awful.popup {
  widget = {
    widget = wibox.container.margin,
    margins = 20,
    prompt,
  },
  ontop = true,
  placement = awful.placement.centered,
  visible = false,
  border_color = beautiful.bg_focus,
  border_width = 2,
  bg = beautiful.bg_dark,
  forced_width = 1000,
  forced_height = 500,
}

function F.prompt.open()
  prompt_widget.visible = true
  prompt:run()
end

function F.prompt.close()
  prompt_widget.visible = false
end
