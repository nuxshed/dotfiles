{ config, pkgs, lib, ... }:
{
  imports = [
    ./dunst.nix
    ./picom.nix
    ./gtk.nix
  ];
  home.packages = with pkgs; [
    tint2
    xss-lock
  ];
  home.file.".config/tint2".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/tint2";

  home.pointerCursor = {
    name = "phinger-cursors";
    package = pkgs.phinger-cursors;
    size = 32;
  };

  xsession = {
    enable = true;
    initExtra = "~/.fehbg\nxss-lock slock &";
  };

}
