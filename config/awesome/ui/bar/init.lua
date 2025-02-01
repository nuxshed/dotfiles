local awful = require "awful"
local wibox = require "wibox"
local beautiful = require "beautiful"
local gears = require "gears"
local rubato = require "modules.rubato"

F = F or {}
F.bar = {}

local slidebar = wibox {
    width = 100,
    height = screen.primary.geometry.height,
    x = 0,  -- Position on the left edge
    y = 0,
    ontop = true,
    visible = true,
    bg = beautiful.bg_normal,
    type = "dock",
}

-- Set struts to reserve space for the slidebar
slidebar:struts {
    left = 100
}

local clock = wibox.widget {
    widget = wibox.container.background,
    bg = beautiful.bg_subtle,
    {
        widget = wibox.container.margin,
        margins = 16,
        {
            layout = wibox.layout.fixed.vertical,
            {
                widget = wibox.widget.textclock,
                format = "%H",
                font = beautiful.font_name .. " Bold 16",
                align = "center",
            },
            {
                widget = wibox.widget.textclock,
                format = "%M",
                font = beautiful.font_name .. " Bold 16",
                align = "center",
            },
        },
    },
}

local bat_arcchart = wibox.widget {
    widget = wibox.container.arcchart,
    start_angle = math.pi / 2,
    thickness = 8,
    value = 100,
    min_value = 0,
    max_value = 100,
    colors = { beautiful.fg_normal },
    bg = beautiful.bg_focus,
}

local battery_circle = wibox.widget {
    value = 0,
    forced_width = 100,
    forced_height = 100,
    widget = wibox.container.radialprogressbar,
    color = beautiful.fg_normal,
    border_color = beautiful.bg_focus,
}

local battery_widget = wibox.widget {
    bg = beautiful.bg_subtle,
    widget = wibox.container.background,
    {
        bat_arcchart,
        widget = wibox.container.margin,
        margins = 15,
    },
}

local layoutbox = wibox.widget {
    bg = beautiful.bg_subtle,
    widget = wibox.container.background,
    buttons = {
        awful.button({}, 4, function()
            awful.layout.inc(1)
        end),
        awful.button({}, 5, function()
            awful.layout.inc(-1)
        end),
    },
    {
        widget = wibox.container.margin,
        margins = 20,
        {
            widget = awful.widget.layoutbox,
        },
    },
}

awesome.connect_signal("squeal::battery", function(capacity)
    local fill_color = beautiful.fg_normal
    bat_arcchart.value = capacity
    bat_arcchart.colors = { fill_color }
    battery_circle.value = capacity / 100
    battery_circle.color = fill_color
end)

screen.connect_signal("request::desktop_decoration", function(s)
    -- Create a tasklist widget
    local tasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons,
        style   = {
            shape = gears.shape.rounded_bar,
        },
        layout   = {
            spacing = 12,
            layout  = wibox.layout.fixed.vertical
        },
        widget_template = {
            {
                {
                    {
                        id     = 'icon_role',
                        widget = wibox.widget.imagebox,
                    },
                    margins = 15,
                    widget  = wibox.container.margin,
                },
                layout = wibox.layout.fixed.horizontal,
            },
            left  = 12,
            right = 12,
            widget = wibox.container.margin,
            bg = beautiful.bg_subtle,
            widget = wibox.container.background,
            create_callback = function(self, c, index, objects)
                self.bg = c == client.focus and beautiful.bg_focus or beautiful.bg_subtle
                c:connect_signal("focus", function()
                    self.bg = beautiful.bg_focus
                end)
                c:connect_signal("unfocus", function()
                    self.bg = beautiful.bg_subtle
                end)
            end,
        },
    }

    -- Setup the slidebar layout
    slidebar:setup {
        {
            {
                {
                    tasklist,
                    layout = wibox.layout.fixed.vertical,
                    spacing = 10,
                },
                margins = 5,
                widget = wibox.container.margin,
            },
            nil,
            {
                {
                    battery_widget,
                    clock,
                    layoutbox,
                    layout = wibox.layout.fixed.vertical,
                    spacing = 20,
                },
                layout = wibox.container.place,
                valign = "bottom",
            },
            layout = wibox.layout.align.vertical,
            expand = "none",
        },
        margins = 15,
        widget = wibox.container.margin,
    }
end)

local bar_slide = rubato.timed {
    pos = 0,
    rate = 120,
    intro = 0.3, 
    duration = 0.6,
    easing = rubato.quadratic,
    awestore_compat = true,
    subscribed = function(pos)
        slidebar.x = pos
        if pos == -100 then
            slidebar:struts { left = 0 }
        elseif pos == 0 then
            slidebar:struts { left = 100 }
        end
    end,
}

function F.bar.toggle()
    if slidebar.x < 0 then
        bar_slide.target = 0
    else
        bar_slide.target = -100
    end
end