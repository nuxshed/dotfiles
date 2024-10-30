local gears = require "gears"
local wibox = require "wibox"
local beautiful = require "beautiful"
local cairo = require("lgi").cairo
local awful = require "awful"

local config = {
  diameter = 300,
  inner_radius_ratio = 0.5,
  border_width = 4,
  animation_speed = 0.05,
  sector_padding = 0.1,
  max_radius = 150,
}

local menu = {
  is_visible = false,
  anim_progress = 0,
  anim_timer = nil,
}

local sectors_data = {
  {
    start_angle = 0,
    end_angle = math.pi / 2,
    icon = "globe",
    callback = function()
      awful.spawn.easy_async("firefox", function() end)
    end,
  },
  {
    start_angle = math.pi / 2,
    end_angle = math.pi,
    icon = "code",
    callback = function()
      awful.spawn.easy_async("emacs", function() end)
    end,
  },
  {
    start_angle = math.pi,
    end_angle = 3 * math.pi / 2,
    icon = "terminal",
    callback = function()
      awful.spawn.easy_async("alacritty", function() end)
    end,
  },
  {
    start_angle = 3 * math.pi / 2,
    end_angle = 2 * math.pi,
    icon = "edit",
    callback = function()
      awful.spawn.easy_async("obsidian", function() end)
    end,
  },
}

local icons = {}

local icons_path = gears.filesystem.get_configuration_dir() .. "/icons/"

for _, sector in ipairs(sectors_data) do
  icons[sector.icon] =
    gears.surface.load_uncached(icons_path .. sector.icon .. ".svg")
end

local function create_sector_widget(sector)
  local widget = wibox.widget {
    widget = wibox.widget.base.make_widget,

    draw = function(self, _, cr, width, height)
      local outer_radius = config.max_radius * menu.anim_progress
      local inner_radius = outer_radius * config.inner_radius_ratio

      cr:translate(width / 2, height / 2)

      local adjusted_start = sector.start_angle + config.sector_padding
      local adjusted_end = sector.end_angle - config.sector_padding

      cr:arc(0, 0, outer_radius, adjusted_start, adjusted_end)
      cr:arc_negative(0, 0, inner_radius, adjusted_end, adjusted_start)
      cr:close_path()

      cr:set_source(gears.color(beautiful.bg_subtle))
      cr:fill_preserve()

      cr:set_source(gears.color(beautiful.bg_focus))
      cr:set_line_width(config.border_width)
      cr:stroke()

      local icon = icons[sector.icon]
      if icon then
        local icon_size = inner_radius * 0.4
        local sector_middle = (adjusted_start + adjusted_end) / 2
        local icon_radius = (inner_radius + outer_radius) / 2
        local icon_x = icon_radius * math.cos(sector_middle)
        local icon_y = icon_radius * math.sin(sector_middle)

        cr:save()
        cr:translate(icon_x, icon_y)
        cr:rotate(sector_middle + math.pi / 2)
        cr:scale(icon_size / icon:get_width(), icon_size / icon:get_height())

        cr:set_source(gears.color(beautiful.fg_normal))
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

local menu_wibox = wibox {
  width = config.diameter,
  height = config.diameter,
  bg = "#00000000",
  ontop = true,
  visible = false,
}

local layout = wibox.layout.manual()
for _, sector in ipairs(sectors_data) do
  layout:add_at(create_sector_widget(sector), { x = 0, y = 0, width = config.diameter, height = config.diameter })
end
menu_wibox:set_widget(layout)

local function update_menu_visibility()
  menu_wibox.visible = menu.is_visible
end

local function hide_menu()
  if menu.is_visible then
    menu.anim_progress = 1
    if menu.anim_timer then
      menu.anim_timer:stop()
    end
    menu.anim_timer = gears.timer {
      timeout = 0.01,
      call_now = true,
      autostart = true,
      callback = function()
        menu.anim_progress = menu.anim_progress - config.animation_speed
        if menu.anim_progress <= 0 then
          menu.anim_progress = 0
          menu.is_visible = false
          update_menu_visibility()
          menu.anim_timer:stop()
          awful.keygrabber.stop(menu_keygrabber)
        end
        layout:emit_signal "widget::redraw_needed"
      end,
    }
  end
end

local function show_menu()
  local mouse = mouse.coords()
  menu_wibox.x = mouse.x - config.diameter / 2
  menu_wibox.y = mouse.y - config.diameter / 2
  menu.is_visible = true
  menu.anim_progress = 0
  update_menu_visibility()

  if menu.anim_timer then
    menu.anim_timer:stop()
  end
  menu.anim_timer = gears.timer {
    timeout = 0.01,
    call_now = true,
    autostart = true,
    callback = function()
      menu.anim_progress = menu.anim_progress + config.animation_speed
      if menu.anim_progress >= 1 then
        menu.anim_progress = 1
        menu.anim_timer:stop()
        menu_keygrabber = awful.keygrabber.run(function(mod, key, event)
          if key == "Escape" and event == "press" then
            hide_menu()
            return true
          end
          return false
        end)
      end
      layout:emit_signal "widget::redraw_needed"
    end,
  }
end

local function toggle_menu()
  if menu.is_visible then
    hide_menu()
  else
    show_menu()
  end
end

menu_wibox:buttons(gears.table.join(awful.button({}, 1, function(_, x, y)
  local dx = x - config.diameter / 2
  local dy = y - config.diameter / 2
  local distance = math.sqrt(dx ^ 2 + dy ^ 2)
  local angle = math.atan2(dy, dx)
  if angle < 0 then
    angle = angle + 2 * math.pi
  end

  local outer_radius = config.max_radius
  local inner_radius = outer_radius * config.inner_radius_ratio

  if distance >= inner_radius and distance <= outer_radius then
    for _, sector in ipairs(sectors_data) do
      if angle >= sector.start_angle and angle <= sector.end_angle then
        sector.callback()
        hide_menu()
        break
      end
    end
  end
end)))

root.buttons(gears.table.join(awful.button({}, 3, toggle_menu), awful.button({}, 1, hide_menu)))
