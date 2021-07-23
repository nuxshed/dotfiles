vim.g.dashboard_default_executive = "telescope"
vim.g.dashboard_enable_session = 0

vim.g.dashboard_custom_header = {
  " ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗",
  " ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║",
  " ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║",
  " ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║",
  " ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║",
  " ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝",
}

vim.g.dashboard_custom_section = {
  a = {
    description = { "  Find File                 SPC f f" },
    command = "Telescope find_files",
  },
  b = {
    description = { "  Recents                   SPC f o" },
    command = "Telescope oldfiles",
  },
  c = {
    description = { "  Find Word                 SPC f g" },
    command = "Telescope live_grep",
  },
  d = {
    description = { "  New File                  SPC f n" },
    command = "DashboardNewFile",
  },
  e = {
    description = { "  Update Plugins            SPC p u" },
    command = "PackerUpdate",
  },
}

vim.g.dashboard_custom_footer = {
  "lol",
}
