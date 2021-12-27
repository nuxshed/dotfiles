{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    ((emacsPackagesNgGen emacs).emacsWithPackages (epkgs: [
      epkgs.vterm
    ]))
  ];
  home.file.".emacs.d/init.el".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/init.el";
}
