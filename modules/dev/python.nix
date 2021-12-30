{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    python39
    python39Packages.pip
    poetry
    nodePackages.pyright
  ];
}
