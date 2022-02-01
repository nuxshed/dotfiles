{ config, pkgs, libs, ... }:
{
  imports = [ ./bottom.nix ./irssi.nix ./kitty ];
  home.packages = with pkgs; [ brave rofi ];
  home.file.".config/rofi".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/rofi";
}
