local awful = require "awful"
local beautiful = require "beautiful"
local naughty = require "naughty"
local playerctl = require("modules.bling").signal.playerctl.lib()
local wibox = require "wibox"
local rubato = require "modules.rubato"
local helpers = require "helpers"

local gears = require "gears"

F.music = {}

local album_art = wibox.widget {
  widget = wibox.widget.imagebox,
  forced_height = 90,
  forced_width = 90,
  image = beautiful.music_default_icon,
}

local name_widget = wibox.widget {
  markup = "No players",
  widget = wibox.widget.textbox,
}

local title_widget = wibox.widget {
  markup = "Nothing Playing",
  widget = wibox.widget.textbox,
  font = beautiful.font_name .. " Extra Bold 10",
}

local artist_widget = wibox.widget {
  markup = "Nothing Playing",
  widget = wibox.widget.textbox,
}

local progress = wibox.widget {
  widget = wibox.widget.progressbar,
  forced_height = 5,
  forced_width = 100,
  color = beautiful.fg_focus,
  background_color = beautiful.bg_focus,
}

playerctl:connect_signal("metadata", function(_, title, artist, album_path, album, player_name)
  album_art:set_image(gears.surface.load_uncached(album_path))
  title_widget:set_markup_silently(title)
  artist_widget:set_markup_silently(artist)
end)

playerctl:connect_signal("position", function(_, prog, prog_max, _)
  progress.max_value = prog_max
  progress.value = prog
end)

local prev = wibox.widget {
  image = beautiful.music_prev_icon,
  widget = wibox.widget.imagebox,
  forced_height = 25,
  forced_width = 25,
}

local next = wibox.widget {
  image = beautiful.music_next_icon,
  widget = wibox.widget.imagebox,
  forced_height = 25,
  forced_width = 25,
}

local play = wibox.widget {
  image = beautiful.music_play_icon,
  widget = wibox.widget.imagebox,
  forced_height = 25,
  forced_width = 25,
}

playerctl:connect_signal("playback_status", function(_, playing, _)
  if playing then
    play.image = beautiful.music_pause_icon
  else
    play.image = beautiful.music_play_icon
  end
end)

play:buttons(gears.table.join(awful.button({}, 1, function()
  playerctl:play_pause()
end)))

next:buttons(gears.table.join(awful.button({}, 1, function()
  playerctl:next()
end)))

prev:buttons(gears.table.join(awful.button({}, 1, function()
  playerctl:previous()
end)))

local music = awful.popup {
  widget = {
    widget = wibox.container.margin,
    margins = 20,
    right = 30,
    forced_width = 350,
    forced_height = 125,
    {
      layout = wibox.layout.fixed.horizontal,
      spacing = 20,
      album_art,
      {
        layout = wibox.layout.fixed.vertical,
        spacing = 10,
        --{
        --  widget = wibox.container.background,
        --  fg = beautiful.fg_minimize,
        --  name_widget,
        --},
        {
          layout = wibox.layout.fixed.vertical,
          spacing = 3,
          {
            widget = wibox.container.scroll.horizontal,
            step_function = wibox.container.scroll.step_functions.waiting_nonlinear_back_and_forth,
            speed = 25,
            forced_width = 200,
            title_widget,
          },
          {
            widget = wibox.container.scroll.horizontal,
            step_function = wibox.container.scroll.step_functions.waiting_nonlinear_back_and_forth,
            speed = 25,
            forced_width = 200,
            artist_widget,
          },
        },
        {
          widget = wibox.container.place,
          halign = "center",
          {
            layout = wibox.layout.fixed.horizontal,
            spacing = 10,
            prev,
            play,
            next,
          },
        },
        progress,
      },
    },
  },
  border_color = beautiful.fg_minimize,
  border_width = 5,
  bg = beautiful.bg_subtle,
  placement = function(c)
    (awful.placement.bottom_left)(c, { margins = { bottom = 20, left = 70 } })
  end,
  ontop = true,
  visible = false,
}

function F.music.toggle()
  music.visible = not music.visible
end

local music_popup = awful.popup {
  bg = beautiful.bg_normal,
  placement = function(c)
    (awful.placement.bottom_left)(c, { margins = { bottom = 115, left = 20 } })
  end,
  ontop = false,
  widget = wibox.widget {
    bg = beautiful.bg_normal,
    widget = wibox.container.background,
    forced_width = 35,
    forced_height = 35,
    buttons = {
      awful.button({}, 1, function()
        F.music.toggle()
      end),
    },
    {
      widget = wibox.container.margin,
      margins = 7,
      {
        widget = wibox.widget.imagebox,
        image = beautiful.music_default_icon,
      },
    },
  },
}

helpers.add_hover_cursor(music_popup, "hand1")

local slide = rubato.timed {
  pos = -100,
  rate = 60,
  intro = 0.3,
  duration = 0.8,
  easing = rubato.quadratic,
  awestore_compat = true,
  subscribed = function(pos)
    music_popup.x = pos
  end,
}

local popup_status = false

slide.ended:subscribe(function()
  if popup_status then
    music_popup.visible = false
  end
end)

playerctl:connect_signal("playback_status", function(_, playing)
  if playing == true then
    music_popup.visible = true
    slide:set(20)
    popup_status = false
  else
    slide:set(-100)
    popup_status = true
  end
end)
