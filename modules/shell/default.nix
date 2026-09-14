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
    frogmouth
    fzf
    github-cli
    gifsicle
    glow
    gnumake
    groff
    hsetroot
    imagemagick
    jq
    lazygit
    libtool
    lsof
    maim
    man-pages
    man-pages-posix
    mpv
    ncdu
    p7zip
    pamixer
    pandoc
    pfetch
    pinentry-curses
    playerctl
    powertop
    # pactl/pacmd only; the sound server itself is pipewire
    pulseaudio
    (ripgrep.override { withPCRE2 = true; })
    slop
    socat
    tdf
    tesseract
    tmux
    television
    tree
    bat
    unrar
    unzip
    v4l-utils
    wget
    wkhtmltopdf
    xclip
    zip
    zoxide

    # iOS device mounting
    ideviceinstaller
    ifuse
    libimobiledevice
    usbmuxd
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
