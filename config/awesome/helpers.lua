local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local wibox = require "wibox"

local helpers = {}

helpers.rrect = function(radius)
  return function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, radius)
  end
end

helpers.squircle = function(rate, delta)
  return function(cr, width, height)
    gears.shape.squircle(cr, width, height, rate, delta)
  end
end

-- Add a hover cursor to a widget by changing the cursor on
-- mouse::enter and mouse::leave
-- You can find the names of the available cursors by opening any
-- cursor theme and looking in the "cursors folder"
-- For example: "hand1" is the cursor that appears when hovering over
-- links
function helpers.add_hover_cursor(w, hover_cursor)
  local original_cursor = "left_ptr"

  w:connect_signal("mouse::enter", function()
    local w = _G.mouse.current_wibox
    if w then
      w.cursor = hover_cursor
    end
  end)

  w:connect_signal("mouse::leave", function()
    local w = _G.mouse.current_wibox
    if w then
      w.cursor = original_cursor
    end
  end)
end

-- Useful for periodically checking the output of a command that
-- requires internet access.
-- Ensures that `command` will be run EXACTLY once during the desired
-- `interval`, even if awesome restarts multiple times during this time.
-- Saves output in `output_file` and checks its last modification
-- time to determine whether to run the command again or not.
-- Passes the output of `command` to `callback` function.
function helpers.remote_watch(command, interval, output_file, callback)
  local run_the_thing = function()
    -- Pass output to callback AND write it to file
    awful.spawn.easy_async_with_shell(command .. " | tee " .. output_file, function(out)
      callback(out)
    end)
  end

  local timer
  timer = gears.timer {
    timeout = interval,
    call_now = true,
    autostart = true,
    single_shot = false,
    callback = function()
      awful.spawn.easy_async_with_shell("date -r " .. output_file .. " +%s", function(last_update, _, __, exitcode)
        -- Probably the file does not exist yet (first time
        -- running after reboot)
        if exitcode == 1 then
          run_the_thing()
          return
        end

        local diff = os.time() - tonumber(last_update)
        if diff >= interval then
          run_the_thing()
        else
          -- Pass the date saved in the file since it is fresh enough
          awful.spawn.easy_async_with_shell("cat " .. output_file, function(out)
            callback(out)
          end)

          -- Schedule an update for when the remaining time to complete the interval passes
          timer:stop()
          gears.timer.start_new(interval - diff, function()
            run_the_thing()
            timer:again()
          end)
        end
      end)
    end,
  }
end

return helpers
