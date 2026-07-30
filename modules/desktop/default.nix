{ inputs, config, pkgs, lib, ... }: {
  imports = [ ];
  home.packages = with pkgs; [
    eww
    swaybg
    grim
    gnome-calendar
    hyprpaper
    slurp
    libnotify
    picom
    papirus-icon-theme
    rofi
    slock
    swaylock
    # tint2
    wl-clipboard-rs
    xdotool
    xss-lock
  ];

  xsession = {
    enable = true;
    initExtra = ''
      xrandr --output eDP-1 --brightness 0.7
      ~/.fehbg
      xss-lock slock &
      picom &'';
  };

  wayland.windowManager.hyprland = {
    enable = true;
    extraConfig = builtins.readFile ../../config/hypr/hyprland.conf;
  };

  qt = {
    enable = true;
    platformTheme.name = "qtct";
  };

  home.file = {
    ".config/awesome".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/dotfiles/config/awesome";
    "dotfiles/config/awesome/modules/bling".source = inputs.bling.outPath;
    "dotfiles/config/awesome/modules/rubato".source = inputs.rubato.outPath;
  };

  home.file.".ratpoisonrc".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/.ratpoisonrc";

  home.file.".config/eww".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/eww";

  home.file.".config/waybar".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/waybar";

  home.file.".config/quickshell".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/quickshell";

  gtk = {
    enable = true;
    theme.name = "Lounge-night-compact";
    iconTheme.name = "Papirus-Dark";
  };

  home.pointerCursor = {
    name = "capitaine-cursors";
    package = pkgs.capitaine-cursors;
    size = 32;
  };
  home.file = {
    ".config/tint2".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/dotfiles/config/tint2";
  };
}
