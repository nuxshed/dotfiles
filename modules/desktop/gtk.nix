{ config, pkgs, libs, ... }:
{
  home.packages = [ pkgs.lounge-gtk-theme pkgs.paper-icon-theme ];
  gtk = {
    enable = true;
    font = {
      name = "JetBrainsMono Nerd Font";
      size = 10;
    };
    theme.name = "Lounge-night-compact";
    iconTheme.name = "Paper";
  };
}
