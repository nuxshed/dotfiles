local awful = require "awful"
local gears = require "gears"

local function save_current_tags()
  local f = io.open("/tmp/taghist", "w")
  if f then
    for s in screen do
      local selected_tag = s.selected_tag
      if selected_tag then
        f:write(string.format("%d\n", selected_tag.index))
      end
    end
    f:close()
  end
end

awesome.connect_signal("exit", function(restart)
  if restart then
    save_current_tags()
  end
end)

local function restore_tags()
  local f = io.open("/tmp/taghist", "r")
  if f then
    for s in screen do
      local tag_index = tonumber(f:read "*line")
      if tag_index and s.tags[tag_index] then
        s.tags[tag_index]:view_only()
      end
    end
    f:close()
  end
end

gears.timer.delayed_call(function()
  restore_tags()
end)
