{ config, pkgs, libs, ... }:
{
  imports = [
    ./picom.nix
    ./gtk.nix
  ];
  home.packages = with pkgs; [
    dunst
    eww
    skippy-xd
    tint2
  ];
  home.file.".config/tint2".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/tint2";

  home.file.".config/dunst".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/dunst";
}
