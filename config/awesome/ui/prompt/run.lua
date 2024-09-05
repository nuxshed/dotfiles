local awful = require "awful"
local gooey = require "ui.gooey"

F.run = {}

local run = awful.widget.prompt {
  prompt = "",
  done_callback = function()
    F.run.close()
  end,
}

local run_widget = gooey.make_prompt_widget(run, { mode = " Run " })

function F.run.open()
  run_widget.visible = true
  run:run()
end

function F.run.close()
  run_widget.visible = false
end
