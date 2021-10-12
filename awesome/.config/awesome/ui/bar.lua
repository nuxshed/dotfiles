local awful = require "awful"
local wibox = require "wibox"
local beautiful = require "beautiful"
local gears = require "gears"
local bling = require "modules.bling"

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

bling.widget.tag_preview.enable {
  show_client_content = false, -- Whether or not to show the client content
  x = 10, -- The x-coord of the popup
  y = 10, -- The y-coord of the popup
  scale = 0.25, -- The scale of the previews compared to the screen
  honor_padding = false, -- Honor padding when creating widget size
  honor_workarea = false, -- Honor work area when creating widget size
  placement_fn = function(c) -- Place the widget using awful.placement (this overrides x & y)
    awful.placement.bottom_left(c, {
      margins = {
        bottom = 50,
        left = 30,
      },
    })
  end,
}

screen.connect_signal("request::desktop_decoration", function(s)
  local l = awful.layout.suit
  awful.tag(
    { "1", "2", "3", "4", "5", "6", "7", "8", "9" },
    s,
    { l.floating, l.tile, l.tile, l.tile, l.tile, l.tile, l.tile, l.tile, l.tile }
  )
  awful.popup({
    bg = beautiful.none,
    placement = function(c)
      (awful.placement.bottom + awful.placement.maximize_horizontally)(
        c,
        { margins = { bottom = 10, left = 10, right = 20 } }
      )
    end,
    shape = gears.shape.rect,
    widget = {
      {
        {
          {
            widget = awful.widget.taglist {
              screen = s,
              filter = awful.widget.taglist.filter.all,
              buttons = {
                awful.button({}, 1, function(t)
                  t:view_only()
                end),
                awful.button({ modkey }, 1, function(t)
                  if client.focus then
                    client.focus:move_to_tag(t)
                  end
                end),
                awful.button({}, 3, awful.tag.viewtoggle),
                awful.button({ modkey }, 3, function(t)
                  if client.focus then
                    client.focus:toggle_tag(t)
                  end
                end),
                awful.button({}, 4, function(t)
                  awful.tag.viewprev(t.screen)
                end),
                awful.button({}, 5, function(t)
                  awful.tag.viewnext(t.screen)
                end),
              },
              style = {
                shape = function(cr, width, height)
                  gears.shape.squircle(cr, width, height, 1.3, 0)
                end,
              },
              widget_template = {
                {
                  {
                    id = "text_role",
                    widget = wibox.widget.textbox,
                  },
                  left = 7,
                  right = 7,
                  widget = wibox.container.margin,
                },
                id = "background_role",
                widget = wibox.container.background,
                create_callback = function(self, c3, index, objects) --luacheck: no unused args
                  self:connect_signal("mouse::enter", function()
                    if #c3:clients() > 0 then
                      awesome.emit_signal("bling::tag_preview::update", c3)
                      awesome.emit_signal("bling::tag_preview::visibility", s, true)
                    end
                  end)
                  self:connect_signal("mouse::leave", function()
                    awesome.emit_signal("bling::tag_preview::visibility", s, false)

                    if self.has_backup then
                      self.bg = self.backup
                    end
                  end)
                end,
              },
            },
          },
          widget = wibox.container.margin,
          margins = 5,
        },
        widget = wibox.container.background,
        bg = beautiful.bg_normal,
        shape = function(cr, width, height)
          gears.shape.rounded_rect(cr, width, height, 9)
        end,
      },
      {
        widget = wibox.container.place,
        halign = "center",
        {
          widget = awful.widget.tasklist {
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
          },
        },
      },
      {
        {
          bg = beautiful.bg_normal,
          fg = beautiful.fg_bat,
          shape = function(cr, width, height)
            gears.shape.rounded_rect(cr, width, height, 9)
          end,
          widget = wibox.container.background,
          {
            {
              widget = awful.widget.watch("status bat", 30),
            },
            left = 7,
            right = 7,
            top = 5,
            bottom = 5,
            widget = wibox.container.margin,
          },
        },
        {
          bg = beautiful.bg_normal,
          fg = beautiful.fg_time,
          shape = function(cr, width, height)
            gears.shape.rounded_rect(cr, width, height, 9)
          end,
          widget = wibox.container.background,
          {
            widget = wibox.widget.textclock,
          },
        },
        {
          bg = beautiful.bg_normal,
          fg = beautiful.fg_time,
          shape = function(cr, width, height)
            gears.shape.rounded_rect(cr, width, height, 9)
          end,
          widget = wibox.container.background,
          {
            widget = wibox.container.margin,
            margins = 8,
            {
              widget = awful.widget.layoutbox {
                screen = s,
                -- Add buttons, allowing you to change the layout
                buttons = {
                  awful.button({}, 1, function()
                    awful.layout.inc(1)
                  end),
                  awful.button({}, 3, function()
                    awful.layout.inc(-1)
                  end),
                  awful.button({}, 4, function()
                    awful.layout.inc(1)
                  end),
                  awful.button({}, 5, function()
                    awful.layout.inc(-1)
                  end),
                },
              },
            },
          },
        },

        layout = wibox.layout.fixed.horizontal,
        spacing = 10,
      },
      layout = wibox.layout.align.horizontal,
      forced_height = 30,
    },
  }):struts { bottom = 40 }
end)
