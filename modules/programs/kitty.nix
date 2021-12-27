{ config, pkgs, libs, ... }:
{
  home.packages = [ pkgs.kitty ];
  home.file.".config/kitty".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/kitty";
}
