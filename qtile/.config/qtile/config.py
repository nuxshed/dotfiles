#                   __ _
#   ___ ___  _ __  / _(_) __ _   _ __  _   _
#  / __/ _ \| '_ \| |_| |/ _` | | '_ \| | | |
# | (_| (_) | | | |  _| | (_| |_| |_) | |_| |
#  \___\___/|_| |_|_| |_|\__, (_) .__/ \__, |
#                        |___/  |_|    |___/
# config file for the qtile window manager

import os
import subprocess
from typing import List, Text  # noqa: F401

import psutil
from libqtile import bar, hook, layout, qtile, widget
from libqtile.config import Click, Drag, DropDown, Group, Key, Match, ScratchPad, Screen
from libqtile.lazy import lazy

mod = "mod4"
terminal = "kitty"

keys = [
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key(
        [mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"
    ),
    Key(
        [mod, "shift"],
        "l",
        lazy.layout.shuffle_right(),
        desc="Move window to the right",
    ),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key(
        [mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"
    ),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "w", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "shift"], "r", lazy.restart(), desc="Restart Qtile"),
    Key([mod, "shift"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "p", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key([mod], "r", lazy.spawn("rofi -show drun"), desc="rofi"),
    # screenshot
    Key(
        [],
        "Print",
        lazy.spawn("scr screen"),
    ),
    Key(
        ["control"],
        "Print",
        lazy.spawn("scr window"),
    ),
    Key(
        ["shift"],
        "Print",
        lazy.spawn("scr selection"),
    ),
    Key([mod], "Print", lazy.spawn("scr screentoclip")),
    Key(
        [mod, "control"],
        "Print",
        lazy.spawn("scr windowtoclip"),
    ),
    Key(
        [mod, "shift"],
        "Print",
        lazy.spawn("scr selectiontoclip"),
    ),
    # XF86 key bindings
    Key([], "XF86AudioRaiseVolume", lazy.spawn("vol +10%")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("vol -10%")),
    Key(["control"], "XF86AudioRaiseVolume", lazy.spawn("vol +1%")),
    Key(["control"], "XF86AudioLowerVolume", lazy.spawn("vol -1%")),
    Key([], "XF86MonBrightnessUp", lazy.spawn("brightnessctl set +10%")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("brightnessctl set 10%-")),
    Key(["control"], "XF86MonBrightnessUp", lazy.spawn("brightnessctl set +1%")),
    Key(["control"], "XF86MonBrightnessDown", lazy.spawn("brightnessctl set 1%-")),
]

group_names = [
    ("1", {"layout": "floating"}),
    ("2", {"layout": "bsp"}),
    ("3", {"layout": "bsp"}),
    ("4", {"layout": "bsp"}),
    ("5", {"layout": "bsp"}),
    ("6", {"layout": "bsp"}),
    ("8", {"layout": "bsp"}),
    ("9", {"layout": "bsp"}),
]

groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i, (name, kwargs) in enumerate(group_names, 1):
    keys.append(
        Key([mod], str(i), lazy.group[name].toscreen())
    )  # Switch to another group
    keys.append(
        Key([mod, "shift"], str(i), lazy.window.togroup(name))
    )  # Send current window to another group

layouts = [
    layout.Bsp(
        margin=10,
        border_width=2,
        border_focus="#61afef",
        border_normal="#282c34",
    ),
    layout.Floating(border_width=2, border_focus="#61afef"),
    layout.MonadWide(
        margin=5, border_width=2, border_focus="#61afef", border_normal="#282c34"
    ),
    layout.MonadTall(
        margin=5, border_width=2, border_focus="#61afef", border_normal="#282c34"
    ),
    layout.Columns(
        margin=10,
        border_width=2,
        border_focus_stack="#61afef",
        border_focus="#61afef",
        border_normal="#282c34",
    ),
    layout.Max(),
    # TODO: TreeTab seems interesting. work on this later
    layout.TreeTab(
        font="Fira Code Nerd Font",
        fontsize=10,
        section_fontsize=10,
        border_width=2,
        bg_color="#131519",
        active_bg="#282c34",
        active_fg="#abb2bf",
        inactive_bg="#1b1d23",
        inactive_fg="#545862",
        padding_left=5,
        padding_x=5,
        padding_y=5,
        section_top=10,
        section_bottom=20,
        level_shift=8,
        vspace=3,
        panel_width=200,
        sections=["Tabs"],
    ),
    layout.Tile(
        margin=10, border_width=2, border_focus="#61afef", border_normal="#282c34"
    ),
    layout.Zoomy(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Matrix(),
    # layout.RatioTile(),
    # layout.VerticalTile(),
]

widget_defaults = dict(
    font="Fira Code Nerd Font",
    fontsize=11,
    padding=3,
    foreground="#abb2bf",
    background="#131519",
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    active="#abb2bf",
                    inactive="#72777f",
                    margin_x=0,
                    rounded=False,
                    highlight_method="block",
                    this_current_screen_border="#c678dd",
                    urgent_alert_method="text",
                    block_highlight_text_color="#131519",
                ),
                widget.CurrentLayoutIcon(
                    custom_icon_paths=[os.path.expanduser("~/.config/qtile/icons")],
                    scale=0.5,
                ),
                widget.WindowName(),
                widget.Prompt(),
                widget.Spacer(),
                widget.Systray(),
                widget.TextBox(text="|", foreground="#545862"),
                widget.TextBox(text="墳", foreground="#61afef"),
                widget.Volume(foreground="#61afef", update_interval=2),
                widget.TextBox(text="|", foreground="#545862"),
                widget.Battery(
                    charge_char="",
                    discharge_char="",
                    empty_char="",
                    full_char="",
                    format="{char} {percent:2.0%}",
                    foreground="#98c379",
                    update_interval=6,
                ),
                widget.TextBox(text="|", foreground="#545862"),
                widget.Clock(format=" %H:%M  ", foreground="#e06c75"),
            ],
            24,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        # *layout.Floating(border_width=2, border_focus="#61afef"),
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True


# autostart.sh
@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser("~")
    subprocess.call([home + "/.config/qtile/autostart.sh"])


# Window swallowing ;)
# credits to Barbarossa93
@hook.subscribe.client_new
def _swallow(window):
    pid = window.window.get_net_wm_pid()
    ppid = psutil.Process(pid).ppid()
    cpids = {
        c.window.get_net_wm_pid(): wid for wid, c in window.qtile.windows_map.items()
    }
    for i in range(5):
        if not ppid:
            return
        if ppid in cpids:
            parent = window.qtile.windows_map.get(cpids[ppid])
            parent.minimized = True
            window.parent = parent
            return
        ppid = psutil.Process(ppid).ppid()


@hook.subscribe.client_killed
def _unswallow(window):
    if hasattr(window, "parent"):
        window.parent.minimized = False


# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
wmname = "LG3D"
