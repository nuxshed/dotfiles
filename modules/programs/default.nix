{ config, pkgs, libs, ... }:
{
  imports = [ ./kitty.nix ];
  home.packages = [ pkgs.brave pkgs.rofi ];
  home.file.".config/rofi".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/rofi";
}
