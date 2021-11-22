return function(s)
  local awful = require "awful"
  local bling = require "modules.bling"
  local gears = require "gears"
  local wibox = require "wibox"
  bling.widget.task_preview.enable {
    x = 20, -- The x-coord of the popup
    y = 20, -- The y-coord of the popup
    height = 200, -- The height of the popup
    width = 200, -- The width of the popup
    placement_fn = function(c) -- Place the widget using awful.placement (this overrides x & y)
      awful.placement.bottom(c, {
        margins = {
          bottom = 50,
        },
      })
    end,
  }
  return awful.widget.tasklist {
    screen = screen[1],
    filter = awful.widget.tasklist.filter.currenttags,
    buttons = {
      awful.button({}, 1, function(c)
        c:activate { context = "tasklist", action = "toggle_minimization" }
      end),
      awful.button({}, 3, function()
        awful.menu.client_list { theme = { width = 250 } }
      end),
      awful.button({}, 4, function()
        awful.client.focus.byidx(-1)
      end),
      awful.button({}, 5, function()
        awful.client.focus.byidx(1)
      end),
    },
    style = {
      shape = function(cr, width, height)
        gears.shape.squircle(cr, width, height, 1.3, 0)
      end,
    },
    layout = {
      spacing = 5,
      forced_num_cols = 1,
      layout = wibox.layout.grid.horizontal,
    },
    widget_template = {
      {
        {
          id = "clienticon",
          widget = awful.widget.clienticon,
        },
        margins = 5,
        widget = wibox.container.margin,
      },
      id = "background_role",
      forced_width = 30,
      forced_height = 30,
      widget = wibox.container.background,
      create_callback = function(self, c, index, objects) --luacheck: no unused
        self:get_children_by_id("clienticon")[1].client = c
        self:connect_signal("mouse::enter", function()
          awesome.emit_signal("bling::task_preview::visibility", s, true, c)
        end)
        self:connect_signal("mouse::leave", function()
          awesome.emit_signal("bling::task_preview::visibility", s, false, c)
        end)
      end,
    },
  }
end
