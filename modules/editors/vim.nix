{ config, pkgs, lib, ... }:

with lib;
let cfg = config.modules.editors.vim;
in
{
  options.modules.editors.vim = {
    enable = mkEnableOption "vim";
  };

  config = mkIf cfg.enable {
    home.file.".vimrc".text = ''
      " bootstrap vim-plug
      if empty(glob('~/.vim/autoload/plug.vim'))
        silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
      endif

      call plug#begin()
      Plug 'tpope/vim-commentary'
      Plug 'tpope/vim-surround'
      Plug 'sainnhe/gruvbox-material'
      call plug#end()

      set termguicolors
      set background=dark
      let g:gruvbox_material_background = 'hard'
      colorscheme gruvbox-material

      set number
      set showtabline=2
      set cursorline
      set shiftwidth=2
      set expandtab
      set laststatus=0
      set mouse=a

      nnoremap <silent> <C-n> :tabnew<CR>
      nnoremap <silent> <C-Left> :tabprevious<CR>
      nnoremap <silent> <C-Right> :tabnext<CR>
    '';
  };
}
