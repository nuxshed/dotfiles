local awful = require "awful"
local gears = require "gears"
local wibox = require "wibox"
local beautiful = require "beautiful"
local helpers = require "helpers"
local rubato = require "modules.rubato"

screen.connect_signal("request::desktop_decoration", function(s)
  local l = awful.layout.suit
  awful.tag({ "1", "2", "3", "4", "5" }, s, { l.tile, l.tile, l.tile, l.tile, l.tile })
end)

local function create_horizontal_taglist(s)
  return awful.widget.taglist {
    screen = s,
    filter = awful.widget.taglist.filter.all,
    layout = wibox.layout.flex.horizontal,
    widget_template = {
      {
        id = "background_role",
        widget = wibox.container.background,
        bg = beautiful.bg_normal,
        shape = gears.shape.rectangle,
        forced_height = 8,
      },
      id = "margin_role",
      widget = wibox.container.margin,
      margins = 5,
    },
  }
end

local function create_tag_wibox(s)
  local tag_wibox = wibox {
    screen = s,
    width = s.geometry.width,
    height = 20,
    x = s.geometry.x,
    y = s.geometry.y,
    visible = false,
    ontop = true,
    bg = "#000000",
    type = "dock",
  }

  tag_wibox:setup {
    layout = wibox.layout.align.horizontal,
    nil,
    create_horizontal_taglist(s),
    nil,
  }

  return tag_wibox
end

local s = screen.primary
local tag_wibox = create_tag_wibox(s)

local slide = rubato.timed {
  pos = -20,
  rate = 60,
  easing = rubato.quadratic,
  intro = 0.2,
  duration = 0.4,
  subscribed = function(pos)
    tag_wibox.y = s.geometry.y + pos
  end,
}

local hide_timer = gears.timer {
  timeout = 1,
  single_shot = true,
  callback = function()
    slide.target = -20
  end,
}

local function show_wibox()
  slide.target = 0
  tag_wibox.visible = true
  hide_timer:again()
end

s:connect_signal("tag::history::update", show_wibox)
