{ config, pkgs, libs, ... }:
{
  services.picom = {
    enable = true;
    package = pkgs.picom-git;
    fade = true;
    # inactiveDim = "0.05";
    shadow = true;
    extraOptions = ''
      shadow-radius = 40
      shadow-opacity = 0.25
      shadow-offset-x = -40
      shadow-offset-y = -40

      corner-radius = 8
    '';
    shadowExclude = [
      "name = 'Notification'"
      "class_g = 'Conky'"
      "class_g ?= 'Notify-osd'"
      "class_g = 'Cairo-clock'"
      "class_g = 'awesome'"
      "_GTK_FRAME_EXTENTS@:c"
    ];
  };
}
