{ config, pkgs, ... }:
{
  services.xserver = {
    enable = true;
    xkbOptions = "eurosign:e,caps:escape";
    libinput = {
      enable = true;
      naturalScrolling = true;
    };
    displayManager.gdm.enable = true;
 
    windowManager = {
     awesome = {
       enable = true;
       package = pkgs.awesome-git;
     };
     berry.enable = true;
     ratpoison.enable = true;
    };

};
  programs.slock.enable = true;
}
