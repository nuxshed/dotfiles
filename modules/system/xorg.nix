{ config, pkgs, ... }:
{
  services.xserver = {
    enable = true;
    # displayManager.lightdm = {
    #   enable = true;
    #   greeters.gtk.enable = true;
    # };
    displayManager.gdm = {
      enable = true;
      wayland = true;
    };
    libinput.enable = true;
    layout = "gb";
    windowManager = {
      awesome = {
        enable = true;
        package = pkgs.awesome-git;
      };
      i3 = {
        enable = true;
        package = pkgs.i3-gaps;
      };
      berry.enable = true;
      herbstluftwm.enable = true;
    };
  };
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };
}
