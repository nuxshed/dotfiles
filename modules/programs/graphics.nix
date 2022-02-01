{ config, pkgs, lib, ... }:

with lib;
let cfg = config.modules.programs.graphics;
in
{
  options.modules.programs.graphics = {
    aseprite.enable = mkEnableOption "aseprite";
    gimp.enable = mkEnableOption "gimp";
    inkscape.enable = mkEnableOption "inkscape";
    krita.enable = mkEnableOption "krita";
  };

  config = {
    home.packages = [
      (mkIf cfg.aseprite.enable pkgs.aseprite)
      (mkIf cfg.gimp.enable pkgs.gimp)
      (mkIf cfg.inkscape.enable pkgs.inkscape)
      (mkIf cfg.krita.enable pkgs.krita)
    ];
  };
}
