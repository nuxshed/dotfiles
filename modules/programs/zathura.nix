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
        default-bg = "#2a2320";
        default-fg = "#b4a99b";
        recolor-darkcolor = "#b4a99b";
        recolor-lightcolor = "#2a2320";
        statusbar-bg = "#332b26";
        statusbar-fg = "#b4a99b";
        statusbar-home-tilde = true;
      };
    };
  };
}

