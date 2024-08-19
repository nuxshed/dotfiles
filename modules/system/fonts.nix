{ config, pkgs, ... }: {
  fonts.packages = with pkgs; [
    cascadia-code
    ibm-plex
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
