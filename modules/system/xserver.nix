{ config, pkgs, ... }: {
  services.xserver = {
    enable = true;
    xkb.options = "eurosign:e,caps:escape";
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
  services.libinput = {
    enable = true;
    touchpad.naturalScrolling = true;
  };
  programs.slock.enable = true;
}
