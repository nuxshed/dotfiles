{ config, pkgs, libs, ... }:
{
  imports = [
    ./picom.nix
    ./gtk.nix
  ];
  home.packages = with pkgs; [
    tint2
    eww
    skippy-xd
  ];
  home.file.".config/tint2".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/tint2";
}
