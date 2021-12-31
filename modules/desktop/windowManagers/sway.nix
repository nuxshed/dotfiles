{ config, pkgs, libs, ... }:
{
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };
  home.packages = with pkgs; [
    swaylock
    swayidle
    wl-clipboard
    mako
  ];
}
