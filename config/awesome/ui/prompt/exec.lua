local awful = require "awful"
local gooey = require "ui.gooey"

F.exec = {}

local exec = awful.widget.prompt {
  prompt = "exec: ",
  exe_callback = awful.util.eval,
  done_callback = function()
    F.exec.close()
  end,
}

local exec_widget = gooey.make_prompt_widget(exec)

function F.exec.open()
  exec_widget.visible = true
  exec:run()
end

function F.exec.close()
  exec_widget.visible = false
end
