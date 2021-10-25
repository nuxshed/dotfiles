{ config, pkgs, libs, ... }:
{
  home.file = {
    ".config/awesome".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/awesome";
  };
}
