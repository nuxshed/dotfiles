{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    gimp
    inkscape
    krita
  ];
}
