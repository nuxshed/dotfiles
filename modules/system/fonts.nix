{ config, pkgs, ... }: {
  fonts.packages = with pkgs; [
    cascadia-code
    eb-garamond
    ibm-plex
    noto-fonts-emoji-blob-bin
  ];

  fonts.fontconfig = {
    defaultFonts = {
      monospace = [ "Cascadia Code" ];
      sansSerif = [ "IBM Plex Sans" ];
      serif = [ "EB Garamond" ];
      emoji = [ "Blobmoji" ];
    };
  };
}
