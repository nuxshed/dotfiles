{ inputs, config, pkgs, lib, ... }:

with lib;
let cfg = config.modules.desktop.windowManager.awesome;
in
{
  options.modules.desktop.windowManager.awesome = {
    enable = mkEnableOption "awesome";
  };

  config = mkIf cfg.enable {
    home.file = {
      ".config/awesome".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/awesome";
      "dotfiles/config/awesome/modules/bling".source = inputs.bling.outPath;
      "dotfiles/config/awesome/modules/layout-machi".source = inputs.layout-machi.outPath;
    };
  };
}
