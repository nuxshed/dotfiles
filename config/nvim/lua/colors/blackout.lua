local M = {}
local cmd = vim.cmd

-- setup colors
function M.setup()
    vim.cmd [[
        hi clear
        if exists("syntax_on")
            syntax reset
        endif
        colorscheme monochrome
    ]]
end

M.colors = {
    fg = "#d7d5d1",
    bg = "#000000",
    accent = "#d7d5d1",
    lightbg = "#444444",
    fgfaded = "#444444",
    grey = "#050505",
    light_grey = "#262626",
    dark_grey = "#444444",
    bright = "#e7e5e3",
    red = "#ffdfdf",
    green = "#dfffdf",
    blue = "#d7d5d1",
    yellow = "#d7d5d1",
    magenta = "#d7d5d1",
    orange = "#d7d5d1",
    cyan = "#d7d5d1",
}

function M.overrides()
    cmd [[
        hi Normal guibg=#000000 guifg=#d7d5d1
        hi TabLineNorm guibg=NONE guifg=#444444
        hi TabLineSel guibg=#262626 guifg=#d7d5d1
        hi TabLineFill guibg=NONE
        hi LspReferenceText gui=NONE guibg=#444444
        hi LspReferenceWrite gui=NONE guibg=#444444
        hi LspReferenceRead gui=NONE guibg=#444444
        hi TSKeyword gui=italic
        hi TSKeywordFunction gui=italic
        hi TSRepeat gui=italic
        hi TSKeywordOperator gui=italic
        hi TSConditional gui=italic
        hi TSProperty gui=italic
        hi MatchParen guibg=#444444
        hi TelescopeBorder guifg=#444444
        hi TelescopeTitle guifg=#d7d5d1
    ]]
end

return M