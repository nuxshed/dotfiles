{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [ emacs ];
 home.file.".emacs.d/init.el".source =
   config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/init.el";
 home.file.".emacs.d/early-init.el".source =
   config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/early-init.el";
 home.file.".emacs.d/themes".source =
   config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/themes";
 home.file.".emacs.d/lisp".source =
   config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/lisp";
 home.file.".emacs.d/.dir-locals.el".source =
   config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/.dir-locals.el";
 home.file.".emacs.d/scripts".source =
   config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/scripts";
  home.file.".emacs.d/custom.el".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/custom.el";
  home.file.".vimrc".text = ''
    " bootstrap vim-plug
    if empty(glob('~/.vim/autoload/plug.vim'))
      silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
      \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    endif
    call plug#begin()
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-surround'
    Plug 'vim-scripts/pyte'
    Plug 'lilydjwg/colorizer'
    call plug#end()
    set termguicolors
    set background=light
    colorscheme pyte
    set cursorline
    hi clear LineNr
    hi clear CursorLineNr
    hi clear TabLineFill
    hi CursorLine cterm=bold
    set number
    set showtabline=2
    set shiftwidth=2
    set expandtab
    set laststatus=0
    set mouse=a
    nnoremap <silent> <C-n> :tabnew<CR>
    nnoremap <silent> <C-Left> :tabprevious<CR>
    nnoremap <silent> <C-Right> :tabnext<CR>
  '';
} 
