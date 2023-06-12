{ config, pkgs, lib, ... }:
{
  config = {
    home.file = {
      ".config/herbstluftwm".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/herbstluftwm";
    };
  };
}
