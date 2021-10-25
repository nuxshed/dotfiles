{ config, pkgs, libs, ... }:
{
  home.packages = [ pkgs.emacs ];
  home.file.".config/doom".source =
  config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/doom";
}
