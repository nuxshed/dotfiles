local dpi = require("beautiful.xresources").apply_dpi
local gears = require "gears"

local gfs = require "gears.filesystem"
local themes_path = gfs.get_themes_dir()
local theme_path = gfs.get_configuration_dir() .. "/themes/forest/"
local icons_path = gfs.get_configuration_dir() .. "/icons/"

local theme = {}

theme.font = "Cartograph CF Regular 8"
theme.font_name = "Cartograph CF Regular "

theme.bg_normal = "#1d2021"
theme.bg_focus = "#282828"
theme.bg_subtle = "#212526"
theme.bg_urgent = "#1d2021"
theme.bg_minimize = "#1d2021"
theme.bg_dark = "#191513"
theme.bg_systray = theme.bg_normal

theme.fg_normal = "#ddc7a1"
theme.fg_focus = "#ddc7a1"
theme.fg_urgent = "#ddc7a1"
theme.fg_minimize = "#a89984"

theme.useless_gap = dpi(15)
theme.border_width = dpi(0)
theme.border_color_normal = theme.bg_subtle
theme.border_color_active = theme.bg_focus
theme.border_color_marked = theme.bg_subtle

theme.green = "#a9b665"
theme.warn = "#d8a657"
theme.critical = "#ea6962"

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- prompt_[fg|bg|fg_cursor|bg_cursor|font]
-- hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]

theme.titlebar_bg_focus = theme.bg_subtle
theme.titlebar_bg_normal = theme.bg_focus

theme.taglist_fg_empty = theme.fg_minimize

theme.slider_active_color = theme.warn
theme.slider_handle_color = theme.fg_normal

theme.control_button_active_bg = theme.green
theme.control_button_active_fg = theme.bg_normal
theme.control_button_normal_bg = theme.bg_normal
theme.control_button_normal_fg = theme.fg_normal

-- Variables set for theming notifications:
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]

theme.start_icon = icons_path .. "menu.svg"
theme.charge_icon = icons_path .. "charge.svg"
theme.notification_icon = icons_path .. "notification.png"
theme.notification_spacing = 10

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_fg_normal = theme.fg_minimize
theme.menu_fg_focus = "#89b482"
theme.menu_submenu_icon = theme_path .. "submenu.svg"
theme.menu_height = dpi(30)
theme.menu_width = dpi(130)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua

-- Define the image to load
theme.titlebar_close_button_normal = theme_path .. "close.svg"
theme.titlebar_close_button_normal_hover = theme_path .. "close_hover.svg"
theme.titlebar_close_button_focus = theme_path .. "close.svg"
theme.titlebar_close_button_focus_hover = theme_path .. "close_hover.svg"

theme.titlebar_minimize_button_normal = theme_path .. "minimize.svg"
theme.titlebar_minimize_button_normal_hover = theme_path .. "minimize_hover.svg"
theme.titlebar_minimize_button_focus = theme_path .. "minimize.svg"
theme.titlebar_minimize_button_focus_hover = theme_path .. "minimize_hover.svg"

theme.titlebar_maximized_button_normal_inactive = theme_path .. "maximized.svg"
theme.titlebar_maximized_button_normal_inactive_hover = theme_path .. "maximized_hover.svg"
theme.titlebar_maximized_button_focus_inactive = theme_path .. "maximized.svg"
theme.titlebar_maximized_button_focus_inactive_hover = theme_path .. "maximized_hover.svg"
theme.titlebar_maximized_button_normal_active = theme_path .. "maximized.svg"
theme.titlebar_maximized_button_normal_active_hover = theme_path .. "maximized_hover.svg"
theme.titlebar_maximized_button_focus_active = theme_path .. "maximized.svg"
theme.titlebar_maximized_button_focus_active_hover = theme_path .. "maximized_hover.svg"

theme.layout_floating = gears.color.recolor_image(themes_path .. "default/layouts/floatingw.png", theme.fg_normal)
theme.layout_tile = gears.color.recolor_image(themes_path .. "default/layouts/tilew.png", theme.fg_normal)
theme.layout_max = gears.color.recolor_image(themes_path .. "default/layouts/maxw.png", theme.fg_normal)
theme.layout_tilebottom = gears.color.recolor_image(themes_path .. "default/layouts/tilebottomw.png", theme.fg_normal)
theme.layout_spiral = gears.color.recolor_image(themes_path .. "default/layouts/spiralw.png", theme.fg_normal)

theme.tag_preview_widget_border_radius = 0
theme.tag_preview_client_border_radius = 0
theme.tag_preview_client_opacity = 0.5
theme.tag_preview_client_bg = theme.bg_normal
theme.tag_preview_client_border_color = theme.bg_subtle
theme.tag_preview_client_border_width = 3
theme.tag_preview_widget_bg = theme.bg_dark
theme.tag_preview_widget_border_color = theme.bg_focus
theme.tag_preview_widget_border_width = 2
theme.tag_preview_widget_margin = 10

theme.task_preview_widget_border_radius = 0
theme.task_preview_widget_bg = theme.bg_dark
theme.task_preview_widget_border_color = theme.bg_focus
theme.task_preview_widget_border_width = 3
theme.task_preview_widget_margin = 15

theme.tabbar_radius = 0
theme.tabbar_style = "default"
theme.tabbar_size = 40
theme.tabbar_position = "top"

theme.icon_theme = nil

return theme
