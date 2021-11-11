{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    tint2
  ];
  home.file.".config/tint2".source =
  config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/tint2";
}
