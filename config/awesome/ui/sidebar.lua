local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local rubato = require "modules.rubato"
local wibox = require "wibox"
local gooey = require "ui.gooey"
local naughty = require "naughty"

F.sidebar = {}
F.quick_settings = {}

local function make_button(opts)
    return gooey.make_button {
        icon = opts.icon,
        icon_fg = opts.icon_fg or beautiful.fg_normal,
        bg = opts.bg or beautiful.bg_subtle,
        bg_hover = opts.bg_hover or beautiful.bg_focus,
        margins = opts.margins or 25,
        exec = opts.exec,
    }
end

local function make_toggle(icon, default_state, exec)
    local toggle = gooey.make_toggle {
        icon = icon,
        initial_state = default_state,
        icon_fg_off = beautiful.fg_normal,
        icon_fg_on = beautiful.bg_normal,
        bg_off = beautiful.bg_subtle,
        bg_on = beautiful.fg_normal,
        bg_hover = beautiful.bg_focus,
        margins = 25,
        width = 80,
        height = 80,
        exec = exec,
    }

    toggle:connect_signal("button::press", function(_, _, _, button)
        if button == 1 then
            toggle:toggle()
        end
    end)

    return toggle
end

local notifs_empty = wibox.widget {
    {
        {
            widget = wibox.container.place,
            halign = "center",
            valign = "center",
            {
                widget = wibox.widget.textbox,
                markup = "<span foreground='" .. beautiful.fg_minimize .. "'>No Notifications</span>",
                align = "center",
                valign = "center",
            },
        },
        widget = wibox.container.margin,
        margins = 20,
    },
    forced_height = 160,
    widget = wibox.container.background,
    bg = beautiful.bg_normal,
}

local notifs_container = wibox.widget {
    spacing = 10,
    spacing_widget = {
        {
            widget = wibox.container.background,
        },
        top = 2,
        bottom = 2,
        left = 6,
        right = 6,
        widget = wibox.container.margin,
    },
    forced_width = beautiful.notifs_width or 240,
    layout = wibox.layout.fixed.vertical,
}

notifs_container:insert(1, notifs_empty)

local function reset_notifs_container()
    notifs_container:reset()
    notifs_container:insert(1, notifs_empty)
end

local function remove_notif(box)
    notifs_container:remove_widgets(box)
    if #notifs_container.children == 0 then
        notifs_container:insert(1, notifs_empty)
    end
end

local function create_notif(icon, n)
    local time = os.date "%H:%M"

    local icon_widget = icon and {
        {
            image = icon,
            resize = true,
            clip_shape = function(cr, width, height)
                gears.shape.rounded_rect(cr, width, height, 2)
            end,
            halign = "center",
            valign = "center",
            forced_height = 100,
            forced_width = 100,
            widget = wibox.widget.imagebox,
        },
        strategy = "exact",
        height = 100, 
        width = 100,
        widget = wibox.container.constraint,
    } or {
        layout = wibox.layout.fixed.horizontal,
        spacing = 0,
    }

    local box = wibox.widget {
        {
            {
                icon_widget,
                {
                    {
                        {
                            step_function = wibox.container.scroll.step_functions.waiting_nonlinear_back_and_forth,
                            speed = 80,
                            {
                                markup = "<b>" .. n.title .. "</b>",
                                font = beautiful.font_name .. " Bold 14",
                                align = "left",
                                widget = wibox.widget.textbox,
                            },
                            forced_width = icon and 230 or 280, 
                            widget = wibox.container.scroll.horizontal,
                        },
                        nil,
                        {
                            markup = "<span foreground='" .. beautiful.fg_minimize .. "'>" .. time .. "</span>",
                            align = "right",
                            valign = "bottom",
                            font = beautiful.font,
                            widget = wibox.widget.textbox,
                        },
                        expand = "none",
                        layout = wibox.layout.align.horizontal,
                    },
                    {
                        markup = n.message,
                        align = "left",
                        forced_width = icon and 280 or 350,
                        widget = wibox.widget.textbox,
                    },
                    spacing = 10,
                    layout = wibox.layout.fixed.vertical,
                },
                layout = wibox.layout.align.horizontal,
            },
            margins = 30,
            left = icon and 0 or 30,
            widget = wibox.container.margin,
        },
        forced_height = 135,
        widget = wibox.container.background,
        bg = beautiful.bg_normal,
        border_width = 3,
        border_color = beautiful.bg_focus,
    }

    box:buttons(gears.table.join(awful.button({}, 1, function()
        remove_notif(box)
    end)))

    return box
end

naughty.connect_signal("request::display", function(n)
    if #notifs_container.children == 1 and notifs_container.children[1] == notifs_empty then
        notifs_container:reset()
    end

    local appicon = n.icon or n.app_icon or beautiful.notification_icon

    notifs_container:insert(1, create_notif(appicon, n))
end)

naughty.connect_signal("destroyed", function(n)
    local has_notifications = false
    for _, child in ipairs(notifs_container.children) do
        if child ~= notifs_empty then
            has_notifications = true
            break
        end
    end
    if not has_notifications then
        reset_notifs_container()
    end
end)

local notifs = wibox.widget {
    {
        layout = wibox.layout.align.horizontal,
    },
    {
        notifs_container,
        layout = wibox.layout.fixed.vertical,
    },
    spacing = 20,
    layout = wibox.layout.fixed.vertical,
}

local quick_settings_widget = wibox.widget {
    {
        make_toggle("wifi", true, function(state)
            awful.spawn.easy_async("nmcli radio wifi " .. (state and "on" or "off"))
        end),
        make_toggle("bluetooth", false, function(state)
            local cmd = state and "unblock" or "block"
            awful.spawn.easy_async("rfkill " .. cmd .. " bluetooth")
            awful.spawn.with_shell("echo -e 'power " .. (state and "on" or "off") .. "\nquit' | bluetoothctl")
        end),
        make_toggle("sunrise", false, function(state)
            awful.spawn.easy_async("asusctl led-pow-2 keyboard -" .. (state and "a" or "s"))
        end),
        make_toggle("bar-chart-2", true, function(state)
            awful.spawn.easy_async("asusctl slash -" .. (state and "e" or "d"))
        end),
        spacing = 20,
        layout = wibox.layout.fixed.horizontal,
    },
    widget = wibox.container.margin,
    margins = 40,
}

local sidebar = awful.popup {
    widget = {
        layout = wibox.layout.align.horizontal,
        {
            widget = wibox.container.background,
            bg = beautiful.bg_focus,
            forced_width = 15,
        },
        {
            margins = 40,
            widget = wibox.container.margin,
            forced_width = 485,
            {
                layout = wibox.layout.align.vertical,
                {
                    halign = "center",
                    widget = wibox.container.place,
                    {
                        layout = wibox.layout.fixed.vertical,
                        spacing = 20,
                        {
                            widget = wibox.widget.textclock,
                            format = "<span font='" .. beautiful.font_name .. " Bold 84'>%H:%M</span>",
                            align = "center",
                        },
                        {
                            widget = wibox.widget.textclock,
                            format = "<span font='" .. beautiful.font_name .. " 18'>%A, %B %d</span>",
                            align = "center",
                        },
                    },
                    widget = wibox.container.margin,
                    margins = { bottom = 50 },
                },
                notifs,
            },
        },
    },
    placement = function(c)
        local s = awful.screen.focused()
        return awful.placement.right + awful.placement.maximize_vertically(c, {
            parent = s,
            margins = { right = 0 }
        })
    end,
    ontop = true,
    visible = false,
    screen = awful.screen.focused(),
    type = "dock",
    bg = beautiful.bg_normal, 
}

local quick_settings_popup = awful.popup {
    widget = quick_settings_widget,
    placement = function(c)
        local s = awful.screen.focused()
        c.x = sidebar.x + 32
        c.y = sidebar.y + sidebar.height - 180
    end,
    ontop = true,
    visible = false,
    width = sidebar.width - 40,
    height = 500,
    screen = awful.screen.focused(),
    type = "dock",
    bg = beautiful.bg_normal,
}

local slide = rubato.timed {
    pos = awful.screen.focused().geometry.width,  
    rate = 60,
    intro = 0.5,
    duration = 1.0,
    easing = rubato.quadratic,
    awestore_compat = true,
    subscribed = function(pos)
        sidebar.x = pos
        quick_settings_popup.x = pos + 32
    end,
}

F.sidebar.show = function()
    local s = awful.screen.focused()
    sidebar.screen = s
    sidebar.height = s.geometry.height
    sidebar.visible = true
    quick_settings_popup.visible = true
    slide.target = s.geometry.width - sidebar.width
    naughty.suspend()
end

F.sidebar.hide = function()
    slide.target = awful.screen.focused().geometry.width
    gears.timer.start_new(slide.duration, function()
        sidebar.visible = false
        quick_settings_popup.visible = false
        naughty.resume()
        return false
    end)
end

F.sidebar.toggle = function()
    if sidebar.visible then
        F.sidebar.hide()
    else
        F.sidebar.show()
    end
end

return F.sidebar
