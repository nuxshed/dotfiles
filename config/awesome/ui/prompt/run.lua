local awful = require "awful"
local gooey = require "ui.gooey"

F.run = {}

local prompt = awful.widget.prompt {
  prompt = "run: ",
  done_callback = function()
    F.run.close()
  end,
}

local prompt_widget = gooey.make_prompt_widget(prompt)

function F.run.open()
  prompt_widget.visible = true
  prompt:run()
end

function F.run.close()
  prompt_widget.visible = false
end
