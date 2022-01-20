{ config, pkgs, ... }:
{
  fonts.fonts = with pkgs; [
    cascadia-code
    ibm-plex
    (nerdfonts.override { fonts = [ "CascadiaCode" "FiraCode" "Iosevka" "JetBrainsMono" ]; })
    noto-fonts-emoji-blob-bin
  ];

  fonts.fontconfig = {
    defaultFonts = {
      monospace = [ "Cascadia Code" ];
      sansSerif = [ "Cascadia Code" ];
      serif = [ "Cascadia Code" ];
      emoji = [ "Blobmoji" ];
    };
  };
}
