local helpers = require "helpers"
return function(s)
  local awful = require "awful"
  local bling = require "modules.bling"
  local wibox = require "wibox"
  bling.widget.task_preview.enable {
    placement_fn = function(c)
      awful.placement.top(c, {
        margins = {
          top = 50,
        },
      })
    end,
  }

  return awful.widget.tasklist {
    screen = s,
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
      shape = helpers.squircle(1.3, 0),
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
