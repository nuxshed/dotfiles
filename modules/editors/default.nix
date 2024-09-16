{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [ emacs helix ];
  home.file.".emacs.d/init.el".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/emacs/init.el";
  home.file.".emacs.d/early-init.el".source =
    config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/emacs/early-init.el";
  home.file.".emacs.d/themes".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/emacs/themes";
  home.file.".emacs.d/lisp".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/emacs/lisp";
  home.file.".emacs.d/.dir-locals.el".source =
    config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/emacs/.dir-locals.el";
  home.file.".emacs.d/scripts".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/emacs/scripts";
  home.file.".emacs.d/custom.el".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/emacs/custom.el";
  home.file.".vimrc".text = ''
    syntax on
    set number
    set showtabline=2
    set shiftwidth=2
    set expandtab
    set laststatus=0
    nnoremap <silent> <C-n> :tabnew<CR>
    nnoremap <silent> <C-Left> :tabprevious<CR>
    nnoremap <silent> <C-Right> :tabnext<CR>
  '';
}
