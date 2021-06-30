local gl = require("galaxyline")
local utils = require("utils")
local condition = require("galaxyline.condition")
local gls = gl.section
gl.short_line_list = { "packer", "NvimTree", "Outline", "LspTrouble" }

local colors = require("colors/" .. vim.g.colorscheme).colors

local mode_color = function()
  local mode_colors = {
    [110] = colors.magenta,
    [105] = colors.red,
    [99] = colors.yellow,
    [116] = colors.yellow,
    [118] = colors.green,
    [22] = colors.green,
    [86] = colors.green,
    [82] = colors.red,
    [115] = colors.blue,
    [83] = colors.blue,
  }

  local color = mode_colors[vim.fn.mode():byte()]
  if color ~= nil then
    return color
  else
    return colors.blue
  end
end

local space = function(num)
  return function()
    return string.rep(" ", num)
  end
end

local checkwidth = function()
  return utils.has_width_gt(35) and condition.buffer_not_empty()
end

local LspStatus = function()
  if #vim.lsp.get_active_clients() > 0 then
    return require("lsp-status").status()
  end
  return ""
end

-- LEFT
gls.left[1] = {
  ViMode = {
    provider = function()
      local aliases = {
        [110] = "NORMAL",
        [105] = "INSERT",
        [99] = "COMMAND",
        [116] = "TERMINAL",
        [118] = "VISUAL",
        [22] = "V-BLOCK",
        [86] = "V-LINE",
        [82] = "REPLACE",
        [115] = "SELECT",
        [83] = "S-LINE",
      }
      vim.api.nvim_command("hi GalaxyViMode guibg=" .. mode_color())
      local alias = aliases[vim.fn.mode():byte()]
      local mode
      if alias ~= nil then
        if utils.has_width_gt(35) then
          mode = alias
        else
          mode = alias:sub(1, 1)
        end
      else
        mode = vim.fn.mode():byte()
      end
      return "  " .. mode .. " "
    end,
    highlight = { colors.bg, colors.bg, "bold" },
    seperator = "/",
    seperator_highlight = { colors.lightbg, colors.lightbg },
  },
}

gls.left[2] = {
  SPACE1 = {
    provider = {
      function()
        return " "
      end,
    },
    condition = condition.buffer_not_empty,
    highlight = { colors.lightbg, colors.lightbg },
  },
}

gls.left[3] = {
  FileIcon = {
    provider = "FileIcon",
    condition = condition.buffer_not_empty,
    highlight = { colors.blue, colors.lightbg },
  },
}

gls.left[4] = {
  FileName = {
    provider = "FileName",
    condition = condition.buffer_not_empty,
    highlight = { colors.fg, colors.lightbg },
  },
}

gls.left[5] = {
  SPACE2 = {
    provider = {
      function()
        return " "
      end,
    },
    condition = condition.buffer_not_empty,
    highlight = { colors.dark_grey, colors.dark_grey },
  },
}

gls.left[6] = {
  DiffAdd = {
    provider = "DiffAdd",
    condition = checkwidth,
    icon = "  ",
    highlight = { colors.white, colors.dark_grey },
  },
}

gls.left[7] = {
  DiffModified = {
    provider = "DiffModified",
    condition = checkwidth,
    icon = "   ",
    highlight = { colors.grey_fg2, colors.dark_grey },
  },
}

gls.left[8] = {
  DiffRemove = {
    provider = "DiffRemove",
    condition = checkwidth,
    icon = "  ",
    highlight = { colors.grey_fg2, colors.dark_grey },
  },
}

gls.left[9] = {
  DiagnosticError = {
    provider = "DiagnosticError",
    icon = "  ",
    highlight = { colors.red, colors.dark_grey },
  },
}

gls.left[10] = {
  DiagnosticWarn = {
    provider = "DiagnosticWarn",
    icon = " ",
    highlight = { colors.yellow, colors.dark_grey },
  },
}

gls.left[10] = {
  LspStatus = {
    provider = { LspStatus },
    highlight = { colors.fgfaded, colors.dark_grey },
  },
}

gls.right[1] = {
  GitBranch = {
    provider = {
      function()
        return "  "
      end,
      "GitBranch",
      space(1),
    },
    condition = condition.check_git_workspace,
    highlight = { colors.green, colors.dark_grey },
  },
}

gls.right[2] = {
  LinePercent = {
    provider = { space(1), "LinePercent" },
    highlight = { colors.fg, colors.lightbg },
  },
}

gls.short_line_left[1] = {
  shortline_NULL = {
    provider = { space(1) },
    condition = condition.buffer_not_empty,
    highlight = { colors.bg, colors.bg },
  },
}
