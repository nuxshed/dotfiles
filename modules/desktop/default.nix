{ config, pkgs, lib, ... }:
{
  imports = [];
  home.packages = with pkgs; [ dunst lounge-gtk-theme papirus-icon-theme slock tint2 xss-lock  ];

  xsession = {
    enable = true;
    initExtra = "~/.fehbg\nxss-lock slock &";
  };

  gtk = {
    enable = true;
    theme.name = "Lounge-night-compact";
    iconTheme.name = "Papirus-Dark";
  };

  home.pointerCursor = {
    name = "capitaine-cursors";
    package = pkgs.capitaine-cursors;
    size = 32;
  };
  home.file = {
    ".config/herbstluftwm".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/herbstluftwm";
    ".config/tint2".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/tint2";
  };
}
