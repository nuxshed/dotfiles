{ inputs, config, pkgs, lib, ... }:
let
  google-sans-flex = pkgs.stdenvNoCC.mkDerivation {
    pname = "google-sans-flex";
    version = "unstable-2025-10";
    src = pkgs.fetchFromGitHub {
      owner = "end-4";
      repo = "google-sans-flex";
      rev = "251aa5abd30496368f634e54ce2a508fe5a2fdfa";
      hash = "sha256-HMAS0L/Tsqyl1xI16cyIzg9LEb6Dyq91JY4wqFQV9Vs=";
    };
    installPhase = ''
      install -Dm644 *.ttf -t $out/share/fonts/truetype
    '';
  };
  qs-python = pkgs.writeShellScriptBin "qs-python" ''
    exec ${pkgs.python3.withPackages (ps: [ ps.numpy ps.pillow ])}/bin/python3 "$@"
  '';
in {
  imports = [ ./spotlight.nix ];
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    google-sans-flex
    qs-python
    blueman
    swaybg
    grim
    gnome-calendar
    hyprpaper
    hyprpicker
    lxappearance
    qt6.qt5compat
    qt6.qtmultimedia
    qt6.qtwayland
    slurp
    libnotify
    picom
    papirus-icon-theme
    rofi
    slock
    swaylock
    # tint2
    wf-recorder
    wl-clipboard-rs
    wtype
    wl-screenrec
    poppler-utils
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
    # HM generates ~/.config/hypr/hyprland.lua (with the systemd session hooks);
    # this makes it load our Lua config from ~/.config/hypr/config.lua.
    extraConfig = ''require("config")'';
  };

  home.file.".config/hypr/config.lua".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/hypr/hyprland.lua";

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

  xdg.desktopEntries.qs-preview = {
    name = "Preview";
    genericName = "Image Viewer";
    exec = "${config.home.homeDirectory}/dotfiles/bin/qs-preview %f";
    icon = "image-x-generic";
    mimeType = [ "image/png" "image/jpeg" "image/gif" "image/webp" "image/bmp" ];
    noDisplay = false;
    categories = [ "Graphics" "Viewer" ];
  };

  xdg.desktopEntries.qs-sysmon = {
    name = "System Monitor";
    exec = "${config.home.homeDirectory}/dotfiles/bin/qs-sysmon";
    icon = "utilities-system-monitor";
    categories = [ "System" "Monitor" ];
  };

  xdg.mimeApps = {
    enable = true;
    defaultApplications = let
      browser = "zen-beta.desktop";
    in {
      "image/png" = "qs-preview.desktop";
      "image/jpeg" = "qs-preview.desktop";
      "image/gif" = "qs-preview.desktop";
      "image/webp" = "qs-preview.desktop";
      "image/bmp" = "qs-preview.desktop";
      "application/pdf" = "zathura.desktop";
      "text/html" = browser;
      "application/xhtml+xml" = browser;
      "x-scheme-handler/http" = browser;
      "x-scheme-handler/https" = browser;
      "x-scheme-handler/obsidian" = "obsidian.desktop";
      "x-scheme-handler/beeper" = "beepertexts.desktop";
      "x-scheme-handler/claude" = "com.anthropic.Claude.desktop";
      "x-scheme-handler/claude-cli" = "claude-code-url-handler.desktop";
    };
  };

  xdg.configFile."xdg-desktop-portal-termfilechooser/config".text = ''
    [filechooser]
    cmd=${config.home.homeDirectory}/dotfiles/bin/qs-filechooser
    default_dir=$HOME
    create_help_file=0
  '';

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
