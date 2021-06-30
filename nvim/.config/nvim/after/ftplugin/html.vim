" Set indent width to two spaces
setlocal ts=2 sw=2 sts=2

" Fix quirkiness in indentation
setlocal indentkeys-=*<Return>

" Make lines longer, and don't break them automatically
setlocal tw=120 linebreak textwidth=0
setlocal nowrap
setlocal matchpairs+=<:>
