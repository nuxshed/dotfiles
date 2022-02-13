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

  xsession = {
    enable = true;
    pointerCursor = {
      name = "capitaine-cursors";
      package = pkgs.capitaine-cursors;
      size = 32;
    };
    initExtra = "~/.fehbg\nxss-lock slock &";
  };

}
