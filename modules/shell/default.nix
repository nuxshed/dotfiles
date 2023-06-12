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
}
