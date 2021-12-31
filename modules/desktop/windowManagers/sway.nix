{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    swaylock
    swayidle
    wl-clipboard
    mako
  ];
}
