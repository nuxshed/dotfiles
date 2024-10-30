local wibox = require "wibox"
local gears = require "gears"
local beautiful = require "beautiful"
local awful = require "awful"
local gooey = require "ui.gooey"

local brightness_widget = {}

local brightness_display = wibox {
    ontop = true,
    visible = false,
    bg = beautiful.bg_normal,
    border_width = 4,
    border_color = beautiful.bg_focus,
    width = 360,
    height = 60,
}

local brightness_bar = wibox.widget {
    max_value = 100,
    value = 0,
    forced_height = 20,
    forced_width = 340,
    shape = gears.shape.rectangle,
    bar_shape = gears.shape.rectangle,
    color = beautiful.bg_focus,
    background_color = beautiful.bg_minimize,
    widget = wibox.widget.progressbar,
}

brightness_display:setup {
    {
        -- gooey.create_icon_widget("sun", beautiful.fg_minimize),
        brightness_bar,
        spacing = 10,
        layout = wibox.layout.fixed.horizontal,
    },
    margins = 20,
    widget = wibox.container.margin,
}

local current_timer

local function show_brightness()
    brightness_display.visible = true
    awful.placement.bottom(brightness_display, { margins = { bottom = 20 }, parent = awful.screen.focused() })

    if current_timer then
        current_timer:stop()
    end

    current_timer = gears.timer.start_new(3, function()
        brightness_display.visible = false
    end)
end

function brightness_widget.update()
    awful.spawn.easy_async("brightnessctl get", function(stdout)
        local brightness = tonumber(stdout:match("(%d+)"))
        brightness_bar.value = (brightness / 2047) * 100
        show_brightness()
    end)
end

return brightness_widget