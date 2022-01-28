{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    aseprite
    gimp
    inkscape
    krita
  ];
}
