{ config, pkgs, lib, ... }:

with lib;
let cfg = config.modules.desktop.windowManager.herbstluftwm;
in
{
  options.modules.desktop.windowManager.herbstluftwm = {
    enable = mkEnableOption "herbstluftwm";
  };

  config = mkIf cfg.enable {
    home.file = {
      ".config/herbstluftwm".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/herbstluftwm";
    };
  };
}

