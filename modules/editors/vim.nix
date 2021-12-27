{ config, pkgs, libs, ... }:
{
  home.file.".vimrc".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/vim/.vimrc";
}
