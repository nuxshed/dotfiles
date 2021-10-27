{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [ sbcl lispPackages.quicklisp ];
}
