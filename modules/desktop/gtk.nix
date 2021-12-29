{ config, pkgs, libs, ... }:
{
  home.packages = [ pkgs.lounge-gtk-theme ];
  gtk = {
    enable = true;
    font = {
      name = "JetBrainsMono Nerd Font";
      size = 10;
    };
    theme.name = "Lounge-night-compact";
  };
}
