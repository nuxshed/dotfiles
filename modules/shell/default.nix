{ config, pkgs, libs, ... }:
{
  imports = [
    ./dircolors.nix
    ./git.nix
    ./zsh.nix
  ];
  home.packages = with pkgs; [
    acpi
    alsa-utils
    bottom
    brightnessctl
    exa
    fd
    feh
    ffmpeg
    fortune
    fzf
    github-cli
    gnumake
    hsetroot
    lazygit
    lm_sensors
    maim
    ncdu
    pamixer
    pass
    playerctl
    ranger
    (ripgrep.override { withPCRE2 = true; })
    slop
    tmux
    unzip
    wget
    xclip
    zoxide
  ];

  # not big enough for their own modules
  home.file.".bin".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/bin";
  home.file.".tmux.conf".text = import ./tmux.nix;
}
