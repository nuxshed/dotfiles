{ config, pkgs, libs, ... }:
{
  imports = [ ../../programs/foot.nix ];
  home.packages = with pkgs; [
    swaylock
    swayidle
    wl-clipboard
    mako
  ];
}
