{ config, pkgs, libs, ... }:
{
  imports = [ ./git.nix ];
  
  home.packages = with pkgs; [
    acpi alsa-utils bottom brightnessctl exa fd feh ffmpeg
    fzf github-cli gnumake hsetroot lazygit maim ncdu pamixer
    playerctl (ripgrep.override { withPCRE2 = true; }) slop
    tmux unzip wget xclip zoxide
  ];

  home.file.".bin".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/bin";
  home.file.".zsh".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/zsh/.zsh";
  home.file.".zshrc".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/zsh/.zshrc";
  home.file.".zshenv".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/zsh/.zshenv";

}
