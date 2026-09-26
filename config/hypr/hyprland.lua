-- Hyprland config, migrated from hyprlang to Lua.
--
-- Home Manager owns ~/.config/hypr/hyprland.lua (it injects the systemd session
-- start/stop hooks). That file does `require("config")`, which loads this file
-- from ~/.config/hypr/config.lua. Hyprland hot-reloads on save.

local mod = "SUPER"

--------------------------------------------------------------------------------
-- Settings
--------------------------------------------------------------------------------

hl.config({
    input = {
        repeat_rate = 25,
        repeat_delay = 600,
        touchpad = {
            natural_scroll = true,
            scroll_factor = 0.2,
        },
    },

    gestures = {
        workspace_swipe_distance = 300,
        workspace_swipe_cancel_ratio = 0.1,
    },

    misc = {
        disable_hyprland_logo = true,
    },

    general = {
        gaps_in = 15,
        gaps_out = 50,
        border_size = 0,
        layout = "scrolling",
    },

    cursor = {
        inactive_timeout = 3,
    },

    xwayland = {
        force_zero_scaling = true,
    },

    dwindle = {
        force_split = 2,
    },

    decoration = {
        rounding = 10,
        shadow = {
            enabled = false,
            range = 10,
            render_power = 3,
            color = "rgba(1a1a1aee)",
        },
    },

    animations = {
        enabled = true,
    },

    group = {
        col = {
            border_active = "rgba(88888888)",
            border_inactive = "rgba(44444488)",
        },
        groupbar = {
            enabled = true,
            font_family = "sans-serif",
            font_size = 11,
            text_color = "rgba(ffffffff)",
            col = {
                active = "rgba(4a4a4aee)",
                inactive = "rgba(2a2a2aee)",
                locked_active = "rgba(4a4a4aee)",
                locked_inactive = "rgba(2a2a2aee)",
            },
        },
    },

    binds = {
        drag_threshold = 10,
    },
})

--------------------------------------------------------------------------------
-- Animations
--------------------------------------------------------------------------------

hl.curve("easeOutQuint", { type = "bezier", points = { { 0.23, 1 }, { 0.32, 1 } } })
hl.curve("easeInOutCubic", { type = "bezier", points = { { 0.65, 0.05 }, { 0.36, 1 } } })
hl.curve("linear", { type = "bezier", points = { { 0, 0 }, { 1, 1 } } })
hl.curve("almostLinear", { type = "bezier", points = { { 0.5, 0.5 }, { 0.75, 1.0 } } })
hl.curve("quick", { type = "bezier", points = { { 0.15, 0 }, { 0.1, 1 } } })

hl.animation({ leaf = "global", enabled = true, speed = 10, bezier = "default" })
hl.animation({ leaf = "border", enabled = true, speed = 5.39, bezier = "easeOutQuint" })
hl.animation({ leaf = "windows", enabled = true, speed = 4.79, bezier = "easeOutQuint" })
hl.animation({ leaf = "windowsIn", enabled = true, speed = 4.1, bezier = "easeOutQuint", style = "popin 87%" })
hl.animation({ leaf = "windowsOut", enabled = true, speed = 1.49, bezier = "linear", style = "popin 87%" })
hl.animation({ leaf = "fadeIn", enabled = true, speed = 1.73, bezier = "almostLinear" })
hl.animation({ leaf = "fadeOut", enabled = true, speed = 1.46, bezier = "almostLinear" })
hl.animation({ leaf = "fade", enabled = true, speed = 3.03, bezier = "quick" })
hl.animation({ leaf = "layers", enabled = true, speed = 3.81, bezier = "easeOutQuint" })
hl.animation({ leaf = "layersIn", enabled = true, speed = 4, bezier = "easeOutQuint", style = "fade" })
hl.animation({ leaf = "layersOut", enabled = true, speed = 1.5, bezier = "linear", style = "fade" })
hl.animation({ leaf = "fadeLayersIn", enabled = true, speed = 1.79, bezier = "almostLinear" })
hl.animation({ leaf = "fadeLayersOut", enabled = true, speed = 1.39, bezier = "almostLinear" })
hl.animation({ leaf = "workspaces", enabled = true, speed = 3, bezier = "easeOutQuint", style = "slidevert" })

--------------------------------------------------------------------------------
-- Gestures
--------------------------------------------------------------------------------

hl.gesture({ fingers = 2, direction = "down", mods = "ALT", action = "close" })
hl.gesture({ fingers = 3, direction = "up", mods = "ALT", action = "fullscreen" })
hl.gesture({ fingers = 3, direction = "down", mods = "ALT", action = "fullscreen" })
hl.gesture({ fingers = 2, direction = "swipe", mods = mod, action = "move" })
hl.gesture({ fingers = 3, direction = "vertical", action = "workspace" })
hl.gesture({ fingers = 3, direction = "left", action = function() hl.dispatch(hl.dsp.layout("move +col")) end })
hl.gesture({ fingers = 3, direction = "right", action = function() hl.dispatch(hl.dsp.layout("move -col")) end })

--------------------------------------------------------------------------------
-- Keybinds
--------------------------------------------------------------------------------

hl.bind(mod .. " + Return", hl.dsp.exec_cmd("wezterm"))
hl.bind(mod .. " + space", hl.dsp.exec_cmd("qs ipc call spotlight toggle"))
hl.bind(mod .. " + e", hl.dsp.exec_cmd("qs ipc call files toggle"))
hl.bind(mod .. " + SHIFT + Escape", hl.dsp.exec_cmd("qs ipc call sysmon toggle"))
hl.bind(mod .. " + m", hl.dsp.exec_cmd("qs ipc call sysmon open overview"))
hl.bind(mod .. " + n", hl.dsp.exec_cmd("qs ipc call notes toggle"))
hl.bind(mod .. " + SHIFT + p", hl.dsp.exec_cmd("qs ipc call booth toggle"))
hl.bind(mod .. " + o", hl.dsp.exec_cmd("qs ipc call overview toggle"))
hl.bind(mod .. " + SHIFT + c", hl.dsp.exec_cmd("qs ipc call calendar toggle"))
hl.bind(mod .. " + period", hl.dsp.exec_cmd("qs ipc call emoji toggle"))
hl.bind("ALT + Tab", hl.dsp.exec_cmd("qs ipc call switcher next windows"))
hl.bind("ALT + SHIFT + Tab", hl.dsp.exec_cmd("qs ipc call switcher prev windows"))
hl.bind(mod .. " + Tab", hl.dsp.exec_cmd("qs ipc call switcher next workspaces"))
hl.bind(mod .. " + SHIFT + Tab", hl.dsp.exec_cmd("qs ipc call switcher prev workspaces"))
hl.bind(mod .. " + c", hl.dsp.window.close())

-- Workspaces
for i = 1, 9 do
    hl.bind(mod .. " + " .. i, hl.dsp.focus({ workspace = i }))
    hl.bind(mod .. " + SHIFT + " .. i, hl.dsp.window.move({ workspace = i }))
end

hl.bind(mod .. " + SHIFT + Space", hl.dsp.window.float())
hl.bind(mod .. " + f", hl.dsp.window.fullscreen())

-- Move focus
hl.bind(mod .. " + h", hl.dsp.focus({ direction = "l" }))
hl.bind(mod .. " + l", hl.dsp.focus({ direction = "r" }))
hl.bind(mod .. " + k", hl.dsp.focus({ direction = "u" }))
hl.bind(mod .. " + j", hl.dsp.focus({ direction = "d" }))

-- Move window
hl.bind(mod .. " + SHIFT + h", hl.dsp.window.move({ direction = "l" }))
hl.bind(mod .. " + SHIFT + l", hl.dsp.window.move({ direction = "r" }))
hl.bind(mod .. " + SHIFT + k", hl.dsp.window.move({ direction = "u" }))
hl.bind(mod .. " + SHIFT + j", hl.dsp.window.move({ direction = "d" }))

-- Resize window
hl.bind(mod .. " + CTRL + h", hl.dsp.window.resize({ x = -20, y = 0, relative = true }))
hl.bind(mod .. " + CTRL + l", hl.dsp.window.resize({ x = 20, y = 0, relative = true }))
hl.bind(mod .. " + CTRL + k", hl.dsp.window.resize({ x = 0, y = -20, relative = true }))
hl.bind(mod .. " + CTRL + j", hl.dsp.window.resize({ x = 0, y = 20, relative = true }))

-- Scrolling layout
hl.bind(mod .. " + comma", hl.dsp.layout("move -col"))
hl.bind(mod .. " + SHIFT + period", hl.dsp.layout("swapcol r"))
hl.bind(mod .. " + SHIFT + comma", hl.dsp.layout("swapcol l"))
hl.bind(mod .. " + CTRL + period", hl.dsp.layout("colresize +0.1"))
hl.bind(mod .. " + CTRL + comma", hl.dsp.layout("colresize -0.1"))
hl.bind(mod .. " + SHIFT + f", hl.dsp.layout("fit active"))

-- Scratchpad
hl.bind(mod .. " + SHIFT + slash", hl.dsp.window.move({ workspace = "special:magic" }))
hl.bind(mod .. " + slash", hl.dsp.workspace.toggle_special("magic"))

-- Groups
hl.bind(mod .. " + g", hl.dsp.group.toggle())
hl.bind(mod .. " + SHIFT + g", hl.dsp.group.next())

-- Mouse: ALT + LMB drags, or floats on click
hl.bind("ALT + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind("ALT + mouse:272", hl.dsp.window.float(), { mouse = true, click = true })

-- Rotate output
hl.bind("SUPER + ALT + Up", hl.dsp.exec_cmd("hyprctl keyword monitor eDP-1,preferred,auto,1.60,transform,0"))
hl.bind("SUPER + ALT + Right", hl.dsp.exec_cmd("hyprctl keyword monitor eDP-1,preferred,auto,1.60,transform,1"))
hl.bind("SUPER + ALT + Down", hl.dsp.exec_cmd("hyprctl keyword monitor eDP-1,preferred,auto,1.60,transform,2"))
hl.bind("SUPER + ALT + Left", hl.dsp.exec_cmd("hyprctl keyword monitor eDP-1,preferred,auto,1.60,transform,3"))

-- Screenshots (quickshell)
hl.bind(mod .. " + SHIFT + s", hl.dsp.exec_cmd("qs ipc call screenshot region copy"))
hl.bind(mod .. " + CTRL + s", hl.dsp.exec_cmd("qs ipc call screenshot fullscreen"))
hl.bind(mod .. " + ALT + s", hl.dsp.exec_cmd("qs ipc call screenshot region scroll"))

-- Volume
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("pamixer -i 5"), { repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("pamixer -d 5"), { repeating = true })
hl.bind("CTRL + XF86AudioRaiseVolume", hl.dsp.exec_cmd("pamixer -i 5 --allow-boost"), { repeating = true })
hl.bind("CTRL + XF86AudioLowerVolume", hl.dsp.exec_cmd("pamixer -d 5 --allow-boost"), { repeating = true })

-- Brightness
hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd("brightnessctl --device=intel_backlight s +5%"), { repeating = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl --device=intel_backlight s 5%-"), { repeating = true })

-- asusctl
hl.bind("XF86KbdBrightnessUp", hl.dsp.exec_cmd("asusctl -n"))
hl.bind("XF86KbdBrightnessDown", hl.dsp.exec_cmd("asusctl -p"))
hl.bind("XF86Launch3", hl.dsp.exec_cmd("asusctl aura -n"))

-- Session
hl.bind(mod .. " + ALT + l", hl.dsp.exec_cmd("loginctl lock-session"))
hl.bind(mod .. " + SHIFT + r", hl.dsp.exec_cmd("pls home"))
hl.bind(mod .. " + SHIFT + e", hl.dsp.exit())

--------------------------------------------------------------------------------
-- Window rules
--------------------------------------------------------------------------------

-- Quickshell toplevels (e.g. the Preview window) should float, not tile.
hl.window_rule({ match = { class = "org.quickshell" }, float = true, center = true })
hl.window_rule({ match = { class = "org.quickshell", title = "^(Calendar|Focus)$" }, tile = true, maximize = true })

--------------------------------------------------------------------------------
-- Autostart
--------------------------------------------------------------------------------

hl.on("hyprland.start", function()
    hl.exec_cmd("hyprpaper")
    hl.exec_cmd("batd")
    hl.exec_cmd("quickshell")
    hl.exec_cmd("dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP")
end)
