{ config, pkgs, lib, ... }: {

  imports = [ ./themes/ncfiesta.nix ];

  programs.alacritty = {
    enable = true;
    settings = {
      window.padding = {
        x = 30;
        y = 10;
      };
      font = {
        size = 10;
        normal = {
          family = "Cartograph CF";
          style = "Regular";
        };
      };
    };
  };
}
