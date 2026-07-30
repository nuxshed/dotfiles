{ config, pkgs, ... }: {
  services.xserver = {
    enable = true;
    xkb.options = "eurosign:e,caps:escape";

    windowManager = {
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
