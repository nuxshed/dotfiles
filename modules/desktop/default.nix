{ inputs, config, pkgs, lib, ... }: {
  imports = [ ];
  home.packages = with pkgs; [
    dunst
    eww
    swaybg
    waybar
    grim
    slurp
    libnotify
    lounge-gtk-theme
    picom-pijulius
    papirus-icon-theme
    redshift
    rofi-wayland
    slock
    swaylock
    tint2
    wl-clipboard-rs
    xdotool
    xss-lock
  ];

  xsession = {
    enable = true;
    initExtra = ''
      unclutter -idle 1 -root &
      xrandr	--output eDP-1 --brightness 0.7
      xss-lock slock &
      picom &'';
  };

  wayland.windowManager.hyprland = {
    enable = true;
    extraConfig = builtins.readFile ../../config/hypr/hyprland.conf;
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

  home.file.".config/dunst".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/dunst";

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
