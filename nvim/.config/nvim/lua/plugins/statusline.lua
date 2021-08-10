local gl = require "galaxyline"
local utils = require "utils"
local cond = require "galaxyline.condition"
local diagnostic = require "galaxyline.provider_diagnostic"
local gls = gl.section
gl.short_line_list = { "packer", "NvimTree", "Outline", "LspTrouble" }

local colors = require("colors/" .. O.colorscheme).colors

local function mode_color()
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

local function space(num)
  return function()
    return string.rep(" ", num)
  end
end

local function checkwidth()
  return utils.has_width_gt(40) and cond.buffer_not_empty()
end

local function LspStatus()
  if #vim.lsp.get_active_clients() > 0 then
    return require("lsp-status").status()
  end
  return ""
end

local LspCheckDiagnostics = function()
  if
    #vim.lsp.get_active_clients() > 0
    and diagnostic.get_diagnostic_error() == nil
    and diagnostic.get_diagnostic_warn() == nil
    and diagnostic.get_diagnostic_info() == nil
  then
    return " "
  end
  return ""
end

local GetGitRoot = function()
  local git_dir = require("galaxyline.provider_vcs").get_git_dir()
  if not git_dir then
    return ""
  end

  local git_root = git_dir:gsub("/.git/?$", "")
  return git_root:match "^.+/(.+)$"
end

-- LEFT

gls.left[2] = {
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
      vim.api.nvim_command("hi GalaxyRightBar guibg=" .. mode_color())

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

gls.left[3] = {
  SPACE1 = {
    provider = {
      function()
        return " "
      end,
    },
    condition = cond.buffer_not_empty,
    highlight = { colors.lightbg, colors.lightbg },
  },
}

gls.left[4] = {
  FileIcon = {
    provider = "FileIcon",
    condition = cond.buffer_not_empty,
    highlight = { colors.blue, colors.lightbg },
  },
}

gls.left[5] = {
  FileName = {
    provider = "FileName",
    condition = cond.buffer_not_empty,
    highlight = { colors.fg, colors.lightbg },
  },
}

gls.left[6] = {
  CurrentDir = {
    provider = function()
      local dir_name = vim.fn.fnamemodify(vim.fn.getcwd(), ":t")
      if not cond.buffer_not_empty() then
        return "  in  " .. dir_name .. " "
      end
      return " in  " .. dir_name .. " "
    end,
    condition = function()
      return cond.buffer_not_empty and O.statusline.CurrentDir
    end,
    highlight = { colors.light_grey, colors.lightbg },
  },
}

gls.left[7] = {
  SPACE2 = {
    provider = {
      function()
        return " "
      end,
    },
    condition = cond.buffer_not_empty,
    highlight = { colors.dark_grey, colors.dark_grey },
  },
}

gls.left[8] = {
  LspStatus = {
    provider = { LspStatus, LspCheckDiagnostics },
    highlight = { colors.fgfaded, colors.dark_grey },
  },
}

gls.left[9] = {
  DiagnosticWarn = {
    provider = { "DiagnosticWarn" },
    icon = "  ",
    highlight = { colors.orange, colors.dark_grey },
  },
}

gls.left[10] = {
  DiagnosticError = {
    provider = { "DiagnosticError" },
    icon = "  ",
    highlight = { colors.red, colors.dark_grey },
  },
}

gls.left[11] = {
  DiagnosticInfo = {
    provider = { "DiagnosticInfo" },
    icon = "  ",
    highlight = { colors.blue, colors.dark_grey },
  },
}

gls.right[1] = {
  DiffAdd = {
    provider = "DiffAdd",
    condition = function()
      return checkwidth and cond.check_git_workspace
    end,
    icon = "+",
    highlight = { colors.green, colors.dark_grey },
  },
}

gls.right[2] = {
  DiffModified = {
    provider = "DiffModified",
    condition = function()
      return checkwidth and cond.check_git_workspace
    end,
    icon = "~",
    highlight = { colors.blue, colors.dark_grey },
  },
}

gls.right[3] = {
  DiffRemove = {
    provider = "DiffRemove",
    condition = function()
      return checkwidth and cond.check_git_workspace
    end,
    icon = "-",
    highlight = { colors.red, colors.dark_grey },
  },
}

gls.right[4] = {
  GitRoot = {
    provider = {
      space(2),
      GetGitRoot,
      function()
        return " @"
      end,
    },
    condition = function()
      return cond.check_git_workspace and O.statusline.GitRoot
    end,
    highlight = { colors.green, colors.dark_grey },
  },
}

gls.right[5] = {
  GitBranch = {
    provider = {
      space(1),
      function()
        return "  "
      end,
      "GitBranch",
      space(2),
    },
    condition = cond.check_git_workspace,
    highlight = { colors.green, colors.dark_grey },
  },
}

gls.right[6] = {
  BufType = {
    provider = { "FileTypeName", space(2) },
    condition = function()
      return utils.has_width_gt(50)
        and O.statusline.BufType
        and cond.buffer_not_empty()
    end,
    highlight = { colors.fg, colors.dark_grey },
  },
}

gls.right[7] = {
  FileEncoding = {
    provider = { "FileEncode", space(1) },
    condition = function()
      return utils.has_width_gt(50)
        and O.statusline.Encoding
        and cond.buffer_not_empty()
    end,
    highlight = { colors.fg, colors.dark_grey },
  },
}

gls.right[8] = {
  LineInfo = {
    provider = { space(2), "LineColumn", "LinePercent" },
    highlight = { colors.fg, colors.dark_grey },
    condition = cond.buffer_not_empty,
  },
}

gls.right[9] = {
  RightBar = {
    provider = space(1),
    highlight = { colors.bg, colors.bg },
  },
}
