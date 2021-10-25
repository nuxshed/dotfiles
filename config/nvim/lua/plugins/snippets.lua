local ls = require "luasnip"

local snippet = ls.snippet
local text = ls.text_node
local f = ls.function_node
local insert = ls.insert_node

ls.config.set_config {
  history = true,
  region_check_events = "CursorMoved,CursorHold,InsertEnter",
  delete_check_events = "InsertLeave",
  enable_autosnippets = true,
}

ls.snippets = {
  lua = {
    -- credit: akinsho
    snippet({
      trig = "use",
      name = "packer use",
      dscr = {
        "packer use plugin block",
        "e.g.",
        "use {'author/plugin'}",
      },
    }, {
      text "use { '",
      -- Get the author and URL in the clipboard and auto populate the author and project
      f(function(_)
        local default = "author/plugin"
        local clip = vim.fn.getreg "*"
        if not vim.startswith(clip, "https://github.com/") then
          return default
        end
        local parts = vim.split(clip, "/")
        if #parts < 2 then
          return default
        end
        local author, project = parts[#parts - 1], parts[#parts]
        return author .. "/" .. project
      end, {}),
      text "' ",
      insert(2, { ", config = function()", "", "end" }),
      text "}",
    }),
  },
}

require("luasnip.loaders.from_vscode").load {
  paths = vim.fn.stdpath "config" .. "/snippets",
}
