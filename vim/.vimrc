" my .vimrc
" i use vim instead of nvim sometimes

" bootstrap vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

autocmd VimEnter * ""

call plug#begin()
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'sainnhe/gruvbox-material'
Plug 'tmux-plugins/vim-tmux' 
call plug#end()

set termguicolors
set background=dark
let g:gruvbox_material_background = 'hard'
colorscheme gruvbox-material

set number
set showtabline=2
set noruler
set cursorline
set hlsearch
set incsearch
set ignorecase
set smartcase
set tabstop=4
set shiftwidth=2
set expandtab
set laststatus=0
set mouse=a

" mappings
nnoremap <silent> <C-Left> :tabprevious<CR>
nnoremap <silent> <C-Right> :tabNext<CR>
nnoremap <silent> <C-Up> :set laststatus=2<CR>
nnoremap <silent> <C-Down> :set laststatus=0<CR>
