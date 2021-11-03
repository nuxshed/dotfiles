require("buftabline").setup {
  tab_format = " #{i} #{b}#{f} ",
  buffer_id_index = false,
  icon_colors = O.icon_colors,
  start_hidden = false,
  auto_hide = false,
  disable_commands = false,
  go_to_maps = false,
  flags = {
    modified = "+",
  },
  hlgroups = {
    current = "TabLineSel",
    normal = "TabLineNorm",
  },
}

for i = 1, 9 do
  utils.nnoremap("<M-" .. i .. ">", function()
    require("buftabline.commands").buftarget(i, "buffer")
  end)
end
