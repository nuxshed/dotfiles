local awful = require "awful"
local modkey = C.modkey
local machi = require "modules/layout-machi"

-- General Awesome keys
awful.keyboard.append_global_keybindings {
  awful.key({ modkey, "Shift" }, "r", awesome.restart, { description = "reload awesome", group = "awesome" }),
  awful.key({ modkey, "Shift" }, "q", awesome.quit, { description = "quit awesome", group = "awesome" }),
  awful.key({ modkey }, "Return", function()
    awful.spawn(C.terminal)
  end, { description = "open a terminal", group = "launcher" }),
  awful.key({ modkey }, "space", function()
    awful.spawn "rofi -show drun"
  end, { description = "run prompt", group = "launcher" }),
  awful.key({ modkey }, "w", function()
    awful.spawn "rofi -show window"
  end, { description = "show windows", group = "launcher" }),
  awful.key({ modkey }, "x", function()
    F.exec.open()
  end, { description = "lua execute prompt", group = "awesome" }),
  awful.key({ modkey }, "r", function()
    F.run.open()
  end, { description = "run command prompt", group = "awesome" }),
  awful.key({ modkey }, "q", function()
    F.exit.toggle()
  end, { description = "exit popup", group = "awesome" }),
  awful.key({ modkey, "Shift" }, "s", function()
    F.scr.toggle()
  end, { description = "screenshot popup", group = "awesome" }),

  -- Frequently Used
  awful.key({ modkey }, "b", function()
    awful.spawn "brave"
  end),
  awful.key({ modkey }, "e", function()
    awful.spawn "emacs"
  end),
}

-- Utility
awful.keyboard.append_global_keybindings {
  -- Screenshot
  awful.key({}, "Print", function()
    awful.spawn "scr screen"
  end, { description = "run prompt", group = "launcher" }),
  awful.key({ "Shift" }, "Print", function()
    awful.spawn "scr selection"
  end, { description = "run prompt", group = "launcher" }),
  awful.key({ modkey }, "Print", function()
    awful.spawn "scr screentoclip"
  end, { description = "run prompt", group = "launcher" }),
  awful.key({ modkey, "Shift" }, "Print", function()
    awful.spawn "scr selectiontoclip"
  end, { description = "run prompt", group = "launcher" }),

  -- XF86 Keys
  awful.key({}, "XF86AudioLowerVolume", function()
    awful.spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"
  end),
  awful.key({}, "XF86AudioRaiseVolume", function()
    awful.spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"
  end),
  awful.key({}, "XF86MonBrightnessUp", function()
    awful.spawn "brightnessctl s +5%"
  end),
  awful.key({}, "XF86MonBrightnessDown", function()
    awful.spawn "brightnessctl s 5%-"
  end),

  -- Scratch
  awful.key({ modkey }, "s", function()
    Scratch.term:toggle()
  end),
}

-- Tags related keybindings
awful.keyboard.append_global_keybindings {
  awful.key({ modkey }, "Left", awful.tag.viewprev, { description = "view previous", group = "tag" }),
  awful.key({ modkey }, "Right", awful.tag.viewnext, { description = "view next", group = "tag" }),
  awful.key({ modkey }, "Escape", awful.tag.history.restore, { description = "go back", group = "tag" }),
}

-- Focus related keybindings
awful.keyboard.append_global_keybindings {
  awful.key({ modkey }, "j", function()
    awful.client.focus.byidx(1)
  end, { description = "focus next by index", group = "client" }),
  awful.key({ modkey }, "k", function()
    awful.client.focus.byidx(-1)
  end, { description = "focus previous by index", group = "client" }),
  awful.key({ modkey }, "Tab", function()
    awful.client.focus.history.previous()
    if client.focus then
      client.focus:raise()
    end
  end, { description = "go back", group = "client" }),
}

awful.keyboard.append_global_keybindings {
  awful.key {
    modifiers = { modkey },
    keygroup = "numrow",
    description = "only view tag",
    group = "tag",
    on_press = function(index)
      local screen = awful.screen.focused()
      local tag = screen.tags[index]
      if tag then
        tag:view_only()
      end
    end,
  },
  awful.key {
    modifiers = { modkey, "Control" },
    keygroup = "numrow",
    description = "toggle tag",
    group = "tag",
    on_press = function(index)
      local screen = awful.screen.focused()
      local tag = screen.tags[index]
      if tag then
        awful.tag.viewtoggle(tag)
      end
    end,
  },
  awful.key {
    modifiers = { modkey, "Shift" },
    keygroup = "numrow",
    description = "move focused client to tag",
    group = "tag",
    on_press = function(index)
      if client.focus then
        local tag = client.focus.screen.tags[index]
        if tag then
          client.focus:move_to_tag(tag)
        end
      end
    end,
  },
}

-- machi
awful.keyboard.append_global_keybindings {
  awful.key({ modkey }, ",", function()
    machi.switcher.start()
  end, {
    description = "edit the current layout as machi layout",
    group = "layout",
  }),
  awful.key({ modkey }, ".", function()
    machi.default_editor.start_interactive()
  end, {
    description = "edit the current layout as machi layout",
    group = "layout",
  }),
}

client.connect_signal("request::default_mousebindings", function()
  awful.mouse.append_client_mousebindings {
    awful.button({}, 1, function(c)
      c:activate { context = "mouse_click" }
    end),
    awful.button({ modkey }, 1, function(c)
      c:activate { context = "mouse_click", action = "mouse_move" }
    end),
    awful.button({ modkey }, 3, function(c)
      c:activate { context = "mouse_click", action = "mouse_resize" }
    end),
  }
end)

client.connect_signal("request::default_keybindings", function()
  awful.keyboard.append_client_keybindings {
    awful.key({ modkey }, "f", function(c)
      c.fullscreen = not c.fullscreen
      c:raise()
    end, { description = "toggle fullscreen", group = "client" }),
    awful.key({ modkey }, "c", function(c)
      c:kill()
    end, { description = "close", group = "client" }),
    awful.key(
      { modkey, "Control" },
      "space",
      awful.client.floating.toggle,
      { description = "toggle floating", group = "client" }
    ),
    awful.key({ modkey, "Control" }, "Return", function(c)
      c:swap(awful.client.getmaster())
    end, { description = "move to master", group = "client" }),
    awful.key({ modkey }, "t", function(c)
      c.ontop = not c.ontop
    end, { description = "toggle keep on top", group = "client" }),
    awful.key({ modkey }, "m", function(c)
      c.maximized = not c.maximized
      c:raise()
    end, { description = "(un)maximize", group = "client" }),
  }
end)
