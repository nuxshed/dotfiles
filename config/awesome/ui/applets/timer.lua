local awful = require "awful"
local wibox = require "wibox"
local beautiful = require "beautiful"
local gears = require "gears"
local naughty = require "naughty"

F = F or {}
F.applets = F.applets or {}
F.applets.timer = {}

local timer_display = wibox.widget {
    widget = wibox.container.background,
    forced_height = 50,
    forced_width = 50,
    bg = beautiful.bg_normal,
}

local timer_textbox = wibox.widget {
    text = "00:00",
    widget = wibox.widget.textbox,
    align = "center",
    valign = "center",
    font = beautiful.font_name .. " Bold 16",
    forced_width = 140,
}

local start_pause_button = wibox.widget {
    text = "▶",  
    widget = wibox.widget.textbox,
    align = "center",
    valign = "center",
    font = beautiful.font_name .. " Bold 16",
    forced_width = 50,
}

local stop_button = wibox.widget {
    text = "⏹",  
    widget = wibox.widget.textbox,
    align = "center",
    valign = "center",
    font = beautiful.font_name .. " Bold 16",
    forced_width = 50,
}

local top_progressbar = wibox.widget {
    max_value     = 1,
    value         = 0,
    forced_height = 4,    
    forced_width  = 280,  
    color         = beautiful.fg_normal,
    background_color = beautiful.bg_focus,
    widget        = wibox.widget.progressbar,
}

local bottom_progressbar = wibox.widget {
    max_value     = 1,
    value         = 0,
    forced_height = 4,    
    forced_width  = 280,  
    color         = beautiful.fg_normal,
    background_color = beautiful.bg_focus,
    widget        = wibox.widget.progressbar,
}

local left_progressbar = wibox.widget {
    max_value     = 1,
    value         = 0,
    forced_height = 4,  
    forced_width  = 100,    
    color         = beautiful.fg_normal,
    background_color = beautiful.bg_focus,
    widget        = wibox.widget.progressbar,
}

local right_progressbar = wibox.widget {
    max_value     = 1,
    value         = 0,
    forced_height = 4,  
    forced_width  = 100,    
    color         = beautiful.fg_normal,
    background_color = beautiful.bg_focus,
    widget        = wibox.widget.progressbar,
}

local righttop_progressbar = wibox.widget {
    max_value     = 1,
    value         = 0,
    forced_height = 4,    
    forced_width  = 140,  
    color         = beautiful.fg_normal,
    background_color = beautiful.bg_focus,
    widget        = wibox.widget.progressbar,
}

local lefttop_progressbar = wibox.widget {
    max_value     = 1,
    value         = 0,
    forced_height = 4,    
    forced_width  = 140,  
    color         = beautiful.fg_normal,
    background_color = beautiful.bg_focus,
    widget        = wibox.widget.progressbar,
}

local rotated_left_progressbar = wibox.container.rotate(left_progressbar, 'east')
local rotated_right_progressbar = wibox.container.rotate(right_progressbar, 'west')
local rotated_bottom_progressbar = wibox.container.rotate(bottom_progressbar, 'south')

local timer_popup = awful.popup {
    ontop = true,
    visible = false,
    bg = beautiful.bg_normal,
    border_width = 0,
    width = 300,
    height = 100,  
    widget = {
        {
            righttop_progressbar,
            lefttop_progressbar,
            layout = wibox.layout.align.horizontal,
        },
        {
            {
                rotated_left_progressbar,  
                {
                    stop_button,
                    timer_textbox,
                    start_pause_button,
                    layout = wibox.layout.fixed.horizontal,
                    spacing = 10,
                },
                rotated_right_progressbar,  
                layout = wibox.layout.align.horizontal,  
            },
            layout = wibox.layout.align.vertical,  
        },
        rotated_bottom_progressbar,  
        layout = wibox.layout.fixed.vertical,
    }
}

local timer = { minutes = 0, seconds = 0, running = false, total_seconds = 0 }
local timer_instance

local function update_timer_text()
    timer_textbox.text = string.format("%02d:%02d", timer.minutes, timer.seconds)
end

local function update_progressbar()
    if timer.total_seconds > 0 then
        local elapsed_seconds = timer.total_seconds - (timer.minutes * 60 + timer.seconds)
        local progress = elapsed_seconds / timer.total_seconds

        local total_bars = 5
        local progress_per_bar = 1 / total_bars
        local full_bars = math.floor(progress / progress_per_bar)
        local partial_progress = (progress % progress_per_bar) / progress_per_bar

        lefttop_progressbar.value = (full_bars >= 1) and 1 or partial_progress
        right_progressbar.value = (full_bars >= 2) and 1 or (full_bars == 1 and partial_progress or 0)
        bottom_progressbar.value = (full_bars >= 3) and 1 or (full_bars == 2 and partial_progress or 0)
        left_progressbar.value = (full_bars >= 4) and 1 or (full_bars == 3 and partial_progress or 0)
        righttop_progressbar.value = (full_bars >= 5) and 1 or (full_bars == 4 and partial_progress or 0)
    else
        lefttop_progressbar.value = 0
        right_progressbar.value = 0
        bottom_progressbar.value = 0
        left_progressbar.value = 0
        righttop_progressbar.value = 0
    end
end

local function stop_timer()
    if timer_instance then
        timer_instance:stop()
    end
    timer.running = false
    timer.minutes = 0
    timer.seconds = 0
    timer.total_seconds = 0
    update_timer_text()
    update_progressbar()
    start_pause_button.text = "▶"
end

local function toggle_timer()
    if timer.running then
        timer_instance:stop()
        start_pause_button.text = "▶"
    else
        if timer.total_seconds == 0 then
            timer.total_seconds = timer.minutes * 60 + timer.seconds
        end
        timer_instance = gears.timer.start_new(1, function()
            if timer.seconds == 0 then
                if timer.minutes == 0 then
                    stop_timer()
                    naughty.notify({ title = "Timer", text = "Time's up!" })
                    return false
                else
                    timer.minutes = timer.minutes - 1
                    timer.seconds = 59
                end
            else
                timer.seconds = timer.seconds - 1
            end
            update_timer_text()
            update_progressbar()
            return true
        end)
        start_pause_button.text = "⏸"
    end
    timer.running = not timer.running
end

timer_textbox:connect_signal("button::press", function(_, _, _, button)
    if button == 1 then
        timer.minutes = (timer.minutes + 1) % 60
    elseif button == 3 then
        timer.minutes = (timer.minutes - 1) % 60
    elseif button == 5 then  
        timer.seconds = timer.seconds + 1
        if timer.seconds >= 60 then
            timer.seconds = 0
            timer.minutes = (timer.minutes + 1) % 60
        end
    elseif button == 4 then  
        timer.seconds = timer.seconds - 1
        if timer.seconds < 0 then
            timer.seconds = 59
            timer.minutes = (timer.minutes - 1) % 60
        end
    end
    timer.total_seconds = timer.minutes * 60 + timer.seconds
    update_timer_text()
    update_progressbar()
end)

start_pause_button:connect_signal("button::press", function(_, _, _, button)
    if button == 1 then
        toggle_timer()
    end
end)

stop_button:connect_signal("button::press", function(_, _, _, button)
    if button == 1 then
        stop_timer()
    end
end)

function F.applets.timer.toggle()
    timer_popup.visible = not timer_popup.visible
    if timer_popup.visible then
        awful.placement.top(timer_popup, { margins = { top = 20 }, parent = awful.screen.focused() })
    end
end
