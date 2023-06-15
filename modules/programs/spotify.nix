{ pkgs, lib, config, ... }:
let
  flake-compat = builtins.fetchTarball
    "https://github.com/edolstra/flake-compat/archive/master.tar.gz";
  spicetify-nix = (import flake-compat {
    src = builtins.fetchTarball
      "https://github.com/the-argus/spicetify-nix/archive/master.tar.gz";
  }).defaultNix;
  spicePkgs = spicetify-nix.packages.${pkgs.system}.default;
in {
  imports = [ spicetify-nix.homeManagerModule ];
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [ "spotify" ];
  programs.spicetify = {
    enable = true;
    theme = spicePkgs.themes.Default;
    enabledExtensions = with spicePkgs.extensions; [
      adblock
      fullAppDisplay
      keyboardShortcut
      popupLyrics
      shuffle
    ];
    enabledCustomApps = with spicePkgs.apps; [ new-releases lyrics-plus ];

   #colorScheme = "custom";

   #customColorScheme = {
   #  text = "343b58";
   #  subtext = "aaabb5";
   #  sidebar-text = "4b557f";
   #  main = "eaecf2";
   #  sidebar = "c5c7d2";
   #  player = "2a2837";
   #  card = "eaecf2";
   #  shadow = "191724";
   #  selected-row = "bbc0d8";
   #  button = "c5c7d2";
   #  button-active = "c5c7d2";
   #  button-disabled = "a3a4af";
   #  tab-active = "d4d6db";
   #  notification = "ebbcba";
   #  notification-error = "1db954";
   #  misc = "eb6f92";
   #};
  };
}
