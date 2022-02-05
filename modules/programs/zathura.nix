{ config, pkgs, lib, ... }:

with lib;
let cfg = config.modules.programs.zathura;
in
{
  options.modules.programs.zathura = {
    enable = mkEnableOption "zathura";
  };

  config = mkIf cfg.enable {
    programs.zathura = {
      enable = true;
      options = {
        recolor = true;
        default-bg = "#191513";
        default-fg = "#998f83";
        recolor-darkcolor = "#998f83";
        recolor-lightcolor = "#191513";
        statusbar-bg = "#211b19";
        statusbar-fg = "#998f83";
        statusbar-home-tilde = true;
      };
    };
  };
}

