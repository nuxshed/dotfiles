local augroup = require("utils").augroup

augroup("ExternalCommands", {
  {
    -- Open images in an image viewer
    events = { "BufEnter" },
    targets = { "*.png,*.jpg" },
    command = "silent !xdg-open % | :bw",
  },
})

augroup("IncSearchHighlight", {
  {
    -- automatically clear search highlight once leaving the commandline
    events = { "CmdlineEnter" },
    targets = { "[/\\?]" },
    command = ":set hlsearch  | redrawstatus",
  },
  {
    events = { "CmdlineLeave" },
    targets = { "[/\\?]" },
    command = ":set nohlsearch | redrawstatus",
  },
})

augroup("Utilities", {
  {
    events = { "FileType" },
    targets = { "gitcommit", "gitrebase" },
    command = "set bufhidden=delete",
  },
  {
    events = { "FileType" },
    targets = { O.smart_close },
    command = "nnoremap <buffer><silent> q :close<CR>",
  },
})

augroup("ftdetect", {
  {
    events = { "BufRead,BufNewFile" },
    targets = { "*.rasi" },
    command = "set filetype=rasi",
  },
  {
    events = { "BufRead,BufNewFile" },
    targets = { "*.log", "*.LOG", "*_log", "*_LOG", "log", "LOG" },
    command = "set filetype=log",
  },
})
