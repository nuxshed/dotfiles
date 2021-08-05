local utils = require "utils"
local augroup = utils.augroup

-- inspired heavily by akinsho's config

vim.api.nvim_exec(
  [[
   augroup vimrc -- Ensure all autocommands are cleared
   autocmd!
   augroup END
  ]],
  ""
)

augroup("ExternalCommands", {
  {
    -- Open images in an image viewer
    events = { "BufEnter" },
    targets = { "*.png,*.jpg" },
    command = "silent !xdg-open % | :bw",
  },
})

augroup("kitty", {
  -- reset kitty background
  {
    events = { "VimLeave" },
    targets = { "*" },
    command = "lua require('external').kitty.reset_background()",
  },
})

augroup("Utilities", {
  {
    -- @source: https://vim.fandom.com/wiki/Use_gf_to_open_a_file_via_its_URL
    events = { "BufReadCmd" },
    targets = { "file:///*" },
    command = 'exe "bd!|edit ".substitute(expand("<afile>"),"file:/*","","")',
  },
  {
    events = { "FileType" },
    targets = { "gitcommit", "gitrebase" },
    command = "set bufhidden=delete",
  },
  {
    events = { "BufWritePre", "FileWritePre" },
    targets = { "*" },
    command = "silent! call mkdir(expand('<afile>:p:h'), 'p')",
  },
})

-- augroup("Templates", {
--   {
--     events = { "BufNewFile" },
--     targets = { "*.ext" },
--     command = "0r $HOME/.config/nvim/templates/skeleton.ext",
--   },
-- })
