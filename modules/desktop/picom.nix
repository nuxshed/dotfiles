{ config, pkgs, libs, ... }:
{
  home.packages = [ pkgs.picom-git ];
  home.file.".config/picom".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/picom";
}
