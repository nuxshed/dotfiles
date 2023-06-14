{ config, pkgs, ... }:
{
  services.xserver = {
    enable = true;
    layout = "gb";
    xkbOptions = "eurosign:e,caps:escape";
    libinput.enable = true;
    displayManager.gdm.enable = true;
 
    windowManager = {
     awesome = {
       enable = true;
       package = pkgs.awesome-git;
     };
     berry.enable = true;
     herbstluftwm.enable = true;
    };

};
  programs.slock.enable = true;
}
