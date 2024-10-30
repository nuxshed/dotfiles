local gears = require "gears"
local wibox = require "wibox"
local beautiful = require "beautiful"
local cairo = require("lgi").cairo
local awful = require "awful"

-- Configuration
local config = {
  diameter = 420,
  inner_radius_ratio = 0.5,
  border_width = 4,
  animation_speed = 0.08,
  sector_padding = 0.05,
}

-- Menu state
local menu = {
  is_visible = false,
  anim_progress = 0,
  anim_timer = nil,
  current_angle = -math.pi / 2,
}

-- Sector data
local sectors_data = {
  {
    start_angle = -math.pi / 2,
    end_angle = -math.pi / 2 + math.pi / 4,
    icon = "crop",
    callback = function()
      awful.spawn "scr selectiontoclip"
    end,
  },
  {
    start_angle = -math.pi / 2 + math.pi / 4,
    end_angle = 0,
    icon = "camera",
    callback = function()
      awful.spawn "rec /home/$USER/Videos/$(date).mp4"
    end,
  },
  {
    start_angle = 0,
    end_angle = math.pi / 4,
    icon = "lock",
    callback = function()
      awful.spawn "loginctl lock-session"
    end,
  },
  {
    start_angle = math.pi / 4,
    end_angle = math.pi / 2,
    icon = "power",
    callback = function()
      awful.spawn "poweroff"
    end,
  },
}

local icons = {}

local icons_path = gears.filesystem.get_configuration_dir() .. "/icons/"

for _, sector in ipairs(sectors_data) do
  icons[sector.icon] =
    gears.surface.load_uncached(icons_path .. sector.icon .. ".svg")
end

local function create_action_sector(sector)
  local widget = wibox.widget {
    widget = wibox.widget.base.make_widget,

    draw = function(self, _, cr, width, height)
      local outer_radius = width / 2
      local inner_radius = outer_radius * config.inner_radius_ratio
      cr:translate(outer_radius, outer_radius)
      cr:rotate(menu.current_angle)

      local adjusted_start_angle = sector.start_angle + config.sector_padding
      local adjusted_end_angle = sector.end_angle - config.sector_padding

      cr:arc(0, 0, outer_radius, adjusted_start_angle, adjusted_end_angle)
      cr:arc_negative(0, 0, inner_radius, adjusted_end_angle, adjusted_start_angle)
      cr:close_path()

      cr:set_source(gears.color(beautiful.bg_subtle))
      cr:fill_preserve()
      cr:set_source(gears.color(beautiful.bg_focus))
      cr:set_line_width(config.border_width)
      cr:stroke()

      local icon = icons[sector.icon]
      if icon then
        local icon_size = inner_radius * 0.4
        local sector_middle = (adjusted_start_angle + adjusted_end_angle) / 2
        local icon_radius = (inner_radius + outer_radius) / 2
        local icon_x = icon_radius * math.cos(sector_middle)
        local icon_y = icon_radius * math.sin(sector_middle)

        cr:save()
        cr:translate(icon_x, icon_y)
        cr:rotate(sector_middle + math.pi / 2)
        cr:scale(icon_size / icon:get_width(), icon_size / icon:get_height())
        cr:set_source(gears.color(beautiful.fg_focus))
        cr:mask_surface(icon, -icon:get_width() / 2, -icon:get_height() / 2)
        cr:restore()
      end
    end,

    fit = function(_, _, width, height)
      return width, height
    end,
  }

  return widget
end

local action_wibox = wibox {
  width = config.diameter,
  height = config.diameter,
  bg = "#00000000",
  ontop = true,
  visible = false,
}

local layout = wibox.layout.manual()
for _, sector in ipairs(sectors_data) do
  layout:add_at(create_action_sector(sector), { x = 0, y = 0, width = config.diameter, height = config.diameter })
end
action_wibox:set_widget(layout)

action_wibox:geometry {
  x = -config.diameter / 2,
  y = (screen[1].geometry.height - config.diameter) / 2,
}

action_wibox:buttons(gears.table.join(awful.button({}, 1, function()
  local mouse = mouse.coords()
  local widget_x = mouse.x - action_wibox.x
  local widget_y = mouse.y - action_wibox.y
  local dx = widget_x - config.diameter / 2
  local dy = widget_y - config.diameter / 2
  local distance = math.sqrt(dx ^ 2 + dy ^ 2)
  local angle = math.atan2(dy, dx)

  if angle < -math.pi / 2 then
    angle = angle + 2 * math.pi
  end

  local outer_radius = config.diameter / 2
  local inner_radius = outer_radius * config.inner_radius_ratio

  if distance >= inner_radius and distance <= outer_radius then
    for _, sector in ipairs(sectors_data) do
      if angle >= sector.start_angle and angle <= sector.end_angle then
        sector.callback()
      end
    end
  end
end)))

local function animate_menu(show)
  if menu.anim_timer then
    menu.anim_timer:stop()
  end

  local start_angle = show and -math.pi or 0
  local end_angle = show and 0 or math.pi

  menu.current_angle = start_angle
  menu.anim_progress = show and 0 or 1

  menu.anim_timer = gears.timer {
    timeout = 0.01,
    autostart = true,
    callback = function()
      if show then
        menu.current_angle = menu.current_angle + config.animation_speed
        menu.anim_progress = (menu.current_angle + math.pi) / math.pi
      else
        menu.current_angle = menu.current_angle + config.animation_speed
        menu.anim_progress = 1 - menu.current_angle / math.pi
      end

      action_wibox.widget:emit_signal "widget::redraw_needed"

      if (show and menu.current_angle >= end_angle) or (not show and menu.current_angle >= end_angle) then
        menu.current_angle = end_angle
        menu.anim_progress = show and 1 or 0
        menu.anim_timer:stop()
        if not show then
          action_wibox.visible = false
          menu.current_angle = -math.pi / 2
        end
      end
    end,
  }
  menu.anim_timer:start()
end

local function toggle_menu()
  if menu.is_visible then
    animate_menu(false)
  else
    action_wibox.visible = true
    animate_menu(true)
  end
  menu.is_visible = not menu.is_visible
end

awful.key({ "Mod4" }, "q", toggle_menu, { description = "toggle action menu", group = "custom" })
root.keys(gears.table.join(root.keys(), awful.key({ "Mod4" }, "q", toggle_menu)))
