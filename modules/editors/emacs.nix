{ config, pkgs, libs, ... }:
{
  home.packages = [ pkgs.emacs ];
  home.file.".emacs.d/init.el".source =
  config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/init.el";
}
