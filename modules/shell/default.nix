{ config, pkgs, libs, ... }: {
  imports = [ ./git.nix ];

  home.packages = with pkgs; [
    acpi
    alsa-utils
    ast-grep
    bottom
    brightnessctl
    cmake
    eza
    fd
    feh
    ffmpeg-full
    forgejo-cli
    fzf
    github-cli
    gifsicle
    gnumake
    hsetroot
    lazygit
    libtool
    maim
    man-pages
    man-pages-posix
    mpv
    ncdu
    pamixer
    pandoc
    pinentry-curses
    playerctl
    powertop
    (ripgrep.override { withPCRE2 = true; })
    slop
    tmux
    television
    bat
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
  };

  home.file.".bin".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/bin";
  home.file.".zsh".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/zsh/.zsh";
  home.file.".zshenv".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/zsh/.zshenv";
  home.file.".zshrc".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/zsh/.zshrc";
}
