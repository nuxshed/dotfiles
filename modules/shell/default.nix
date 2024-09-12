{ config, pkgs, libs, ... }: {
  imports = [ ./git.nix ];

  home.packages = with pkgs; [
    acpi
    alsa-utils
    bottom
    brightnessctl
    cmake
    eza
    fd
    feh
    ffmpeg-full
    fzf
    github-cli
    gifsicle
    gnumake
    hsetroot
    lazygit
    libtool
    maim
    mpv
    ncdu
    pamixer
    pandoc
    playerctl
    powertop
    (ripgrep.override { withPCRE2 = true; })
    slop
    tmux
    unzip
    wget
    xclip
    zoxide
  ];

  programs = {
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };
    zsh.enable = true;
  };

  home.file.".bin".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/bin";
  home.file.".zsh".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/zsh/.zsh";
  home.file.".zshrc".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/zsh/.zshrc";
  home.file.".zshenv".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/zsh/.zshenv";

}
