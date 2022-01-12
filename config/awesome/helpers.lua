local awful = require "awful"
local gears = require "gears"

local helpers = {}

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
