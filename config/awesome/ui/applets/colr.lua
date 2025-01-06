local awful = require "awful"
local wibox = require "wibox"
local beautiful = require "beautiful"
local gears = require "gears"
local naughty = require "naughty"

F.applets = F.applets or {}
F.applets.colr = {}

local color_display = wibox.widget {
    widget = wibox.container.background,
    forced_height = 40,
    forced_width = 40,
    bg = beautiful.bg_normal,
}

local color_textbox = wibox.widget {
    text = "colr",
    widget = wibox.widget.textbox,
    align = "left",
    valign = "center",
    font = beautiful.font_name .. " Regular 18",
    forced_width = 120,
}

color_textbox:connect_signal("button::press", function(_, _, _, button)
    if button == 1 then
        local color = color_textbox.text
        if color:match("#%x+") then
            awful.spawn.with_shell("echo -n '" .. color .. "' | xclip -selection clipboard")
            naughty.notify({ title = "Color Copied", text = color .. " copied to clipboard." })
        end
    end
end)

local simple_popup = awful.popup {
    ontop = true,
    visible = false,
    bg = beautiful.bg_normal,
    border_width = 4,
    border_color = beautiful.bg_focus,
    width = 250,
    height = 50,
    widget = {
        {
            {
                {
                    color_display,
                    margins = 10,
                    widget = wibox.container.margin,
                },
                color_textbox,
                layout = wibox.layout.fixed.horizontal,
                spacing = 10,
            },
            margins = 20,
            widget = wibox.container.margin,
        },
        layout = wibox.layout.fixed.vertical,
    }
}

function F.applets.colr.toggle()
    if simple_popup.visible then
        simple_popup.visible = false
        awful.spawn.with_shell("pkill xcolor")
        color_textbox.text = "colr"
        color_display.bg = beautiful.bg_normal
    else
        awful.spawn.easy_async("xcolor", function(stdout)
            local color = stdout:match("#%x+")
            if color then
                color_textbox.text = color
                color_display.bg = color
                simple_popup.visible = true
                awful.placement.top(simple_popup, { margins = { top = 20 }, parent = awful.screen.focused() })

                gears.timer.start_new(5, function()
                    simple_popup.visible = false
                    color_textbox.text = "colr"
                    color_display.bg = beautiful.bg_normal
                    return false
                end)
            else
                color_textbox.text = "Failed :/"
                color_display.bg = beautiful.bg_normal
            end
        end)
    end
end