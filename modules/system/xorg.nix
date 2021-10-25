{ config, pkgs, ... }:
{
  services.xserver = {
    enable = true;
    displayManager.gdm.enable = true;
    libinput.enable = true;
    layout = "gb";
    windowManager.awesome = {
      enable = true;
      package = pkgs.awesome-git;
    };
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
    };
  };
}
