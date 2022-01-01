{ config, pkgs, libs, ... }:
{
  home.packages = [ pkgs.sxhkd ];
  home.file = {
    ".config/berry".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/berry";
  };
}
