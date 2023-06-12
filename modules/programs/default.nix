{ config, pkgs, lib, ... }:
{
  imports = [ ./kitty ];
  home.packages = with pkgs; [ brave rofi ];
}
