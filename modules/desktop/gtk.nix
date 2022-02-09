{ config, pkgs, libs, ... }:
{
  home.packages = [ pkgs.lounge-gtk-theme pkgs.papirus-icon-theme ];
  gtk = {
    enable = true;
    font = {
      name = "Commissioner";
      size = 10;
    };
    theme.name = "Lounge-night-compact";
    iconTheme.name = "Papirus-Dark";
  };
}
