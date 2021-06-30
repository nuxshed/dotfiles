" wrapping is expensive don't do it for huge log files
if line('$') > 100000
  setlocal nowrap
  syntax clear
else
  setlocal wrap
endif

setlocal foldmethod=manual
setlocal colorcolumn=
setlocal nolist
setlocal nonumber norelativenumber
setlocal signcolumn=yes:2
