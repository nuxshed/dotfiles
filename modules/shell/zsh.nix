{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    nix-zsh-completions
  ];
  home.file.".zsh".source =
  config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/zsh/.zsh";
  home.file.".zshrc".source =
  config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/zsh/.zshrc";
  home.file.".zshenv".source =
  config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/zsh/.zshenv";
}
