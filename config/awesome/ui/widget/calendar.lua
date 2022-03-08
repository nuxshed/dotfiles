local awful = require "awful"
local beautiful = require "beautiful"
local gears = require "gears"
local wibox = require "wibox"

local styles = {}

styles.month = {
  padding = 20,
}

styles.normal = {
  fg_color = beautiful.fg_dark,
  markup = function(t)
    return "<span font_desc='" .. beautiful.font_name .. " Bold 9" .. "'>" .. t .. "</span>"
  end,
}

styles.focus = {
  fg_color = beautiful.fg_normal,
  bg_color = beautiful.bg_subtle,
  markup = function(t)
    return "<b>" .. t .. "</b>"
  end,
  padding = 5,
}

styles.header = {
  markup = function(t)
    return "<span font_desc='" .. beautiful.font_name .. " Bold 15" .. "'>" .. t .. "</span>"
  end,
}
styles.weekday = {}

local function decorate_cell(widget, flag, date)
  if flag == "monthheader" and not styles.monthheader then
    flag = "header"
  end
  local props = styles[flag] or {}
  if props.markup and widget.get_text and widget.set_markup then
    widget:set_markup(props.markup(widget:get_text()))
  end
  -- Change bg color for weekends
  local default_bg = beautiful.bg_dark
  local ret = wibox.widget {
    {
      widget,
      margins = (props.padding or 2) + (props.border_width or 0),
      widget = wibox.container.margin,
    },
    shape = props.shape,
    fg = props.fg_color or beautiful.fg_normal,
    bg = props.bg_color or default_bg,
    widget = wibox.container.background,
  }
  return ret
end

local calendar_widget = wibox.widget {
  date = os.date "*t",
  spacing = 15,
  fn_embed = decorate_cell,
  widget = wibox.widget.calendar.month,
}

local calendar = awful.popup {
  widget = {
    widget = wibox.container.margin,
    margins = 7,
    calendar_widget,
  },
  bg = beautiful.bg_dark,
  visible = false,
  border_width = 2,
  border_color = beautiful.bg_focus,
  placement = function(c)
    (awful.placement.bottom_left)(c, { margins = { left = 65, bottom = 10 } })
  end,
  ontop = true,
}

local current_month = os.date("*t").month
calendar_widget:buttons(gears.table.join(
  -- Left Click - Reset date to current date
  awful.button({}, 1, function()
    calendar_widget.date = os.date "*t"
  end),
  -- Scroll - Move to previous or next month
  awful.button({}, 4, function()
    new_calendar_month = calendar_widget.date.month - 1
    if new_calendar_month == current_month then
      calendar_widget.date = os.date "*t"
    else
      calendar_widget.date = { month = new_calendar_month, year = calendar_widget.date.year }
    end
  end),
  awful.button({}, 5, function()
    new_calendar_month = calendar_widget.date.month + 1
    if new_calendar_month == current_month then
      calendar_widget.date = os.date "*t"
    else
      calendar_widget.date = { month = new_calendar_month, year = calendar_widget.date.year }
    end
  end)
))

local function toggle()
  calendar.visible = not calendar.visible
end

return toggle
