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
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-abolish'
Plug 'preservim/nerdtree'
Plug 'sainnhe/gruvbox-material'
" Plug 'b4skyx/serenade'
" Plug 'arcticicestudio/nord-vim'
" Plug 'joshdick/onedark.vim'
" Plug 'morhetz/gruvbox'
call plug#end()

set termguicolors
set background=dark
let g:gruvbox_material_background = 'hard'
colorscheme gruvbox-material

set number
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

nnoremap <C-n> :NERDTreeToggle<CR>
