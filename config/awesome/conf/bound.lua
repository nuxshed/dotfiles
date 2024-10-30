local awful = require "awful"
local modkey = C.modkey or "Mod4"
local altmod = C.altmod or "Mod1"

-- General Awesome keys
awful.keyboard.append_global_keybindings {
  awful.key({ modkey, "Shift" }, "r", awesome.restart, { description = "reload awesome", group = "awesome" }),
  awful.key({ modkey, "Shift" }, "q", awesome.quit, { description = "quit awesome", group = "awesome" }),
  awful.key({ modkey }, "Return", function()
    awful.spawn.easy_async(C.terminal, function() end)
  end, { description = "open a terminal", group = "launcher" }),
  awful.key({ modkey }, "space", function()
    awful.spawn.easy_async("rofi -show drun", function() end)
  end, { description = "run prompt", group = "launcher" }),
  awful.key({ modkey }, "w", function()
    awful.spawn.easy_async("rofi -show window", function() end)
  end, { description = "show windows", group = "launcher" }),
  awful.key({ modkey }, "Escape", function()
    F.splash.toggle()
  end, { description = "toggle splash", group = "awesome" }),
  awful.key({ modkey }, "x", function()
    F.exec.open()
  end, { description = "lua execute prompt", group = "awesome" }),
  awful.key({ modkey }, "r", function()
    F.run.open()
  end, { description = "run command prompt", group = "awesome" }),
  awful.key({ modkey }, "q", function()
    F.exit.toggle()
  end, { description = "exit popup", group = "awesome" }),
  awful.key({ modkey }, "a", function()
    F.action.toggle()
  end, { description = "action center", group = "awesome" }),
  awful.key({ modkey }, "s", function()
    F.notifs.toggle()
  end, { description = "notification center", group = "awesome" }),
}

-- Frequently Used Applications
awful.keyboard.append_global_keybindings {
  awful.key({ modkey }, "b", function()
    awful.spawn.easy_async(C.browser, function() end)
  end, { description = "open browser", group = "applications" }),
  awful.key({ modkey }, "e", function()
    awful.spawn.easy_async(C.editor, function() end)
  end, { description = "open editor", group = "applications" }),
  awful.key({ modkey }, "z", function()
    awful.spawn.easy_async("zathura", function() end)
  end, { description = "open zathura", group = "applications" }),
  awful.key({ modkey }, "n", function()
    awful.spawn.easy_async(C.notes, function() end)
  end, { description = "open notes", group = "applications" }),
}

-- Utility
awful.keyboard.append_global_keybindings {
  -- Screenshot
  awful.key({}, "Print", function()
    awful.spawn.easy_async("scr screen", function() end)
  end, { description = "screenshot screen", group = "utility" }),
  awful.key({ "Shift" }, "Print", function()
    awful.spawn.easy_async("scr selection", function() end)
  end, { description = "screenshot selection", group = "utility" }),
  awful.key({ modkey }, "Print", function()
    awful.spawn.easy_async("scr screentoclip", function() end)
  end, { description = "screenshot to clipboard", group = "utility" }),
  awful.key({ modkey, "Shift" }, "Print", function()
    awful.spawn.easy_async("scr selectiontoclip", function() end)
  end, { description = "screenshot selection to clipboard", group = "utility" }),


  -- XF86 Keys
  awful.key({}, "XF86AudioLowerVolume", function()
    awful.spawn.easy_async("amixer set Master 5%-", function() end)
  end, { description = "lower volume", group = "media" }),
  awful.key({}, "XF86AudioRaiseVolume", function()
    awful.spawn.easy_async("amixer set Master 5%+", function() end)
  end, { description = "raise volume", group = "media" }),
  awful.key({}, "XF86MonBrightnessUp", function()
    awful.spawn.easy_async("brightnessctl s +5%", function() end)
  end, { description = "increase brightness", group = "media" }),
  awful.key({}, "XF86MonBrightnessDown", function()
    awful.spawn.easy_async("brightnessctl s 5%-", function() end)
  end, { description = "decrease brightness", group = "media" }),

  -- Scratch
  awful.key({ altmod }, "s", function()
    Scratch.term:toggle()
  end, { description = "toggle scratch terminal", group = "utility" }),
}

-- Screen Rotation
awful.keyboard.append_global_keybindings {
  awful.key({ modkey, "Control" }, "Left", function()
    awful.spawn.easy_async("xrandr -o 1", function() end)
  end, { description = "rotate screen left", group = "screen" }),
  awful.key({ modkey, "Control" }, "Right", function()
    awful.spawn.easy_async("xrandr -o 3", function() end)
  end, { description = "rotate screen right", group = "screen" }),
  awful.key({ modkey, "Control" }, "Up", function()
    awful.spawn.easy_async("xrandr -o 0", function() end)
  end, { description = "rotate screen normal", group = "screen" }),
  awful.key({ modkey, "Control" }, "Down", function()
    awful.spawn.easy_async("xrandr -o 2", function() end)
  end, { description = "rotate screen inverted", group = "screen" }),
}

-- Tags related keybindings
awful.keyboard.append_global_keybindings {
  awful.key({ modkey }, "Left", awful.tag.viewprev, { description = "view previous tag", group = "tag" }),
  awful.key({ modkey }, "Right", awful.tag.viewnext, { description = "view next tag", group = "tag" }),
  awful.key({ modkey }, "Escape", awful.tag.history.restore, { description = "go back to previous tag", group = "tag" }),
}

-- Client management
awful.keyboard.append_global_keybindings {
  awful.key({ modkey }, "j", function()
    awful.client.focus.byidx(1)
  end, { description = "focus next client", group = "client" }),
  awful.key({ modkey }, "k", function()
    awful.client.focus.byidx(-1)
  end, { description = "focus previous client", group = "client" }),
  awful.key({ modkey }, "Tab", function()
    awful.client.focus.history.previous()
    if client.focus then
      client.focus:raise()
    end
  end, { description = "go back to previous client", group = "client" }),
}

-- Client swapping and layout adjustments
awful.keyboard.append_global_keybindings {
  awful.key({ modkey, "Shift" }, "j", function()
    awful.client.swap.byidx(1)
  end, { description = "swap with next client", group = "client" }),
  awful.key({ modkey, "Shift" }, "k", function()
    awful.client.swap.byidx(-1)
  end, { description = "swap with previous client", group = "client" }),
  awful.key({ modkey }, "u", awful.client.urgent.jumpto, { description = "jump to urgent client", group = "client" }),
  awful.key({ modkey }, "l", function()
    awful.tag.incmwfact(0.05)
  end, { description = "increase master width factor", group = "layout" }),
  awful.key({ modkey }, "h", function()
    awful.tag.incmwfact(-0.05)
  end, { description = "decrease master width factor", group = "layout" }),
  awful.key({ modkey, "Shift" }, "h", function()
    awful.tag.incnmaster(1, nil, true)
  end, { description = "increase number of master clients", group = "layout" }),
  awful.key({ modkey, "Shift" }, "l", function()
    awful.tag.incnmaster(-1, nil, true)
  end, { description = "decrease number of master clients", group = "layout" }),
  awful.key({ modkey, "Control" }, "h", function()
    awful.tag.incncol(1, nil, true)
  end, { description = "increase number of columns", group = "layout" }),
  awful.key({ modkey, "Control" }, "l", function()
    awful.tag.incncol(-1, nil, true)
  end, { description = "decrease number of columns", group = "layout" }),
  awful.key({ altmod, "Shift" }, "space", function()
    awful.layout.inc(1)
  end, { description = "select next layout", group = "layout" }),
  awful.key({ modkey, "Shift" }, "space", function()
    awful.layout.inc(-1)
  end, { description = "select previous layout", group = "layout" }),
}

-- Tag management
awful.keyboard.append_global_keybindings {
  awful.key {
    modifiers = { modkey },
    keygroup = "numrow",
    description = "view only tag",
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

-- Mouse bindings
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

-- Client keybindings
client.connect_signal("request::default_keybindings", function()
  awful.keyboard.append_client_keybindings {
    awful.key({ modkey }, "f", function(c)
      c.fullscreen = not c.fullscreen
      c:raise()
    end, { description = "toggle fullscreen", group = "client" }),
    awful.key({ modkey }, "c", function(c)
      c:kill()
    end, { description = "close client", group = "client" }),
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
    end, { description = "(un)maximize client", group = "client" }),
  }
end)

-- Global mouse bindings
awful.mouse.append_global_mousebindings {
  awful.button({}, 3, function()
    M.main:toggle()
  end),
  awful.button({}, 4, awful.tag.viewprev),
  awful.button({}, 5, awful.tag.viewnext),
}
