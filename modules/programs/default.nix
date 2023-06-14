{ config, pkgs, lib, ... }:
{
  imports = [ ./kitty ];
  home.packages = with pkgs; [ brave font-manager rofi ];
}
