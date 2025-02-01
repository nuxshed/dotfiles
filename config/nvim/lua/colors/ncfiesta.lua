local M = {}
local cmd = vim.cmd

-- setup colors
function M.setup()
    vim.cmd [[
        colorscheme no-clown-fiesta
    ]]
end

M.colors = {
    fg = "#E1E1E1",
    bg = "#151515",
    accent = "#202020",
    lightbg = "#171717",
    fgfaded = "#AFAFAF",
    grey = "#373737",
    light_grey = "#727272",
    dark_grey = "#151515",
    bright = "#E1E1E1",
    red = "#b46958",
    green = "#90A959",
    blue = "#BAD7FF",
    yellow = "#F4BF75",
    magenta = "#AA759F",
    orange = "#FFA557",
    cyan = "#88afa2",
}

function M.overrides()
    cmd [[
        hi Normal guibg=#151515 guifg=#E1E1E1
        hi TabLineNorm guibg=NONE guifg=#373737
        hi TabLineSel guibg=#171717 guifg=#E1E1E1
        hi TabLineFill guibg=NONE
        hi LspReferenceText gui=NONE guibg=#373737
        hi LspReferenceWrite gui=NONE guibg=#373737
        hi LspReferenceRead gui=NONE guibg=#373737
        hi TSKeyword gui=italic
        hi TSKeywordFunction gui=italic
        hi TSRepeat gui=italic
        hi TSKeywordOperator gui=italic
        hi TSConditional gui=italic
        hi TSProperty gui=italic
        hi MatchParen guibg=#373737
        hi TelescopeBorder guifg=#373737
        hi TelescopeTitle guifg=#E1E1E1
    ]]
end

return M