{ config, pkgs, lib, ... }:

with lib;
let cfg = config.modules.desktop.windowManager.sway;
in
{
  options.modules.desktop.windowManager.sway = {
    enable = mkEnableOption "sway";
  };

  imports = [ ../../programs/foot.nix ];

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      swaylock
      swayidle
      wl-clipboard
      mako
    ];
  };
}
