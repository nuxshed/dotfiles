{ inputs, config, pkgs, lib, ... }:
let
  # Hyprland composites on the Intel iGPU (aquamarine picks card1/i915 as primary
  # drm, with the NVIDIA card registered only as a secondary). Spotify is CEF and,
  # as a native Wayland client via NIXOS_OZONE_WL, hands its buffers straight to
  # the compositor. Left to itself it picks the NVIDIA EGL driver and exports
  # dmabufs with NVIDIA modifiers that Intel mesa cannot import:
  #
  #   [EGL] eglCreateImageKHR errored out with EGL_BAD_MATCH: createImageFromDmaBufs failed
  #
  # Hyprland does not handle that failure, so the following eglCreateSyncKHR
  # aborts inside mesa (dri_create_fence_fd -> SIGABRT) and takes the whole
  # session down. Hiding the NVIDIA ICD from Spotify keeps it on mesa/Intel.
  spotify-igpu = pkgs.symlinkJoin {
    name = "spotify-igpu";
    paths = [ pkgs.spotify ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/spotify \
        --set __EGL_VENDOR_LIBRARY_FILENAMES /run/opengl-driver/share/glvnd/egl_vendor.d/50_mesa.json \
        --set __GLX_VENDOR_LIBRARY_NAME mesa \
        --set DRI_PRIME 0 \
        --set LIBVA_DRIVER_NAME iHD \
        --unset __NV_PRIME_RENDER_OFFLOAD
    '';
  };
in {
  home.packages = with pkgs; [
    inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.default
    inputs.claude-desktop-extra.packages.${pkgs.stdenv.hostPlatform.system}.default
    antigravity
    beeper
    deluge-gtk
    discord
    firefox
    foliate
    font-manager
    foot
    inkscape
    obsidian
    postman
    qbittorrent
    spotify-igpu
    thunderbird
    vlc
    wezterm
    xcolor
    xdotool
  ];
  imports = [ ./alacritty inputs.spicetify-nix.homeManagerModules.default ];

  nixpkgs.config = { allowUnfree = true; };

  home.file.".config/rofi".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/rofi";
  home.file.".config/wezterm".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/wezterm";
  home.file.".mozilla/firefox/oq8rnh56.default/chrome".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/firefox";

  programs.zathura = {
    enable = true;
    options = {
      recolor = true;
      default-bg = "#141414";
      default-fg = "#c6c6c6";
      recolor-darkcolor = "#c6c6c6";
      recolor-lightcolor = "#141414";
      statusbar-home-tilde = true;
      guioptions = "none";
      clipboard = "selection-clipboard";
      scroll-step = 100;
    };
    mappings = {
      "j" = "feedkeys <C-Down>";
      "k" = "feedkeys <C-Up>";
    };
  };


# programs.spicetify =
# let
#   spicePkgs = inputs.spicetify-nix.legacyPackages.${pkgs.system};
# in
# {
#   enable = true;
#
#   enabledExtensions = with spicePkgs.extensions; [
#     hidePodcasts shuffle keyboardShortcut powerBar showQueueDuration lastfm volumePercentage
#   ];
#
#   enabledCustomApps = with spicePkgs.apps; [
#     marketplace
#     ncsVisualizer
#   ];
# };

}
