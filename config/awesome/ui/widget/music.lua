local gears = require "gears"
local awful = require "awful"
local wibox = require "wibox"
local beautiful = require "beautiful"

local art = wibox.widget {
  image = beautiful.not_playing,
  resize = true,
  forced_height = 80,
  widget = wibox.widget.imagebox,
  halign = "center",
  clip_shape = function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, 5)
  end,
}

local title_widget = wibox.widget {
  font = beautiful.font_name .. " Bold 9",
  markup = "Not Playing",
  align = "center",
  widget = wibox.widget.textbox,
}

local artist_widget = wibox.widget {
  markup = "No Artist",
  align = "center",
  widget = wibox.widget.textbox,
}

playerctl = require("modules.bling").signal.playerctl.lib()
playerctl:connect_signal("metadata", function(_, title, artist, art_path, album, new, player_name)
  -- Set art widget
  art:set_image(gears.surface.load_uncached(art_path))

  -- Set player name, title and artist widgets
  title_widget:set_markup_silently(title)
  artist_widget:set_markup_silently(artist)
end)

local slider = wibox.widget {
  forced_height = 4,
  bar_shape = function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, 5)
  end,
  shape = function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, 5)
  end,
  background_color = beautiful.fg_dark,
  color = beautiful.sidebar_music_progress_fg,
  value = 69,
  max_value = 100,
  widget = wibox.widget.progressbar,
}

playerctl:connect_signal("position", function(pos, length, _)
  slider.value = (pos / length) * 100
end)

local music = wibox.widget {
  {
    {
      art,
      widget = wibox.container.margin,
      top = 10,
      bottom = 15,
    },
    title_widget,
    artist_widget,
    -- {
    --   slider,
    --   widget = wibox.container.margin,
    --   top = 20,
    --   left = 20,
    --   right = 20,
    -- },
    widget = wibox.layout.fixed.vertical,
    spacing = 5,
  },
  widget = wibox.container.margin,
  margins = 5,
}

return music
