{ inputs, config, pkgs, libs, ... }:
{
  home.file = {
    ".config/awesome".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/awesome";
    "dotfiles/config/awesome/modules/bling".source = inputs.bling.outPath;
    "dotfiles/config/awesome/modules/layout-machi".source = inputs.layout-machi.outPath;
  };
}
