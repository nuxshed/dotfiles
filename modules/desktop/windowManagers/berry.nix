{ config, pkgs, lib, ... }:

with lib;
let cfg = config.modules.desktop.windowManager.berry;
in
{
  options.modules.desktop.windowManager.berry = {
    enable = mkEnableOption "berry";
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.sxhkd ];
    home.file = {
      ".config/berry".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/berry";
    };
  };
}
