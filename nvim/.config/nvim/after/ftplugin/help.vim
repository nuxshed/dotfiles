setlocal nonumber norelativenumber nolist
setlocal colorcolumn=
setlocal concealcursor=nc

""---------------------------------------------------------------------------//
" Credit: Tweekmonster
""---------------------------------------------------------------------------//
" if this a vim help file rather than one I'm creating
" add mappings otherwise do not
if expand('%') =~# '^'.$VIMRUNTIME || &readonly
  autocmd BufWinEnter <buffer> wincmd L | vertical resize 80
  nnoremap <buffer> q :<c-u>q<cr>
  nnoremap <buffer> <CR> <C-]>
  nnoremap <buffer> <BS> <C-T>
  nnoremap <silent><buffer> o /'\l\{2,\}'<CR>
  nnoremap <silent><buffer> O ?'\l\{2,\}'<CR>
  nnoremap <silent><buffer> s /\|\zs\S\+\ze\|<CR>
  nnoremap <silent><buffer> S ?\|\zs\S\+\ze\|<CR>
  finish
else
  setlocal spell spelllang=en_gb
endif

setlocal formatexpr=HelpFormatExpr()

nnoremap <silent><buffer> <leader>r :<c-u>call <sid>right_align()<cr>
nnoremap <silent><buffer> <leader>ml maGovim:tw=78:ts=8:noet:ft=help:norl:<esc>`a

if exists('*HelpFormatExpr')
  finish
endif


function! s:right_align() abort
  let text = matchstr(getline('.'), '^\s*\zs.\+\ze\s*$')
  let remainder = (&l:textwidth + 1) - len(text)
  call setline(line('.'), repeat(' ', remainder).text)
  undojoin
endfunction


function! HelpFormatExpr() abort
  if mode() ==# 'i' || v:char != ''
    return 1
  endif

  let line = getline(v:lnum)
  if line =~# '^=\+$'
    normal! macc
    normal! 78i=
    normal! `a
    undojoin
    return
  elseif line =~# '^\k\%(\k\|\s\)\+\s*\*\%(\k\|-\)\+\*\s*'
    let [header, link] = split(line, '^\k\%(\k\|\s\)\+\zs\s*')
    let header = substitute(header, '^\_s*\|\_s*$', '', 'g')
    let remainder = (&l:textwidth + 1) - len(header) - len(link)
    let line = header.repeat(' ', remainder).link
    call setline(v:lnum, line)
    return
  endif

  return 1
endfunction
