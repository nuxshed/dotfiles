{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    ((emacsPackagesNgGen emacsGit).emacsWithPackages (epkgs: [
      epkgs.vterm
    ]))
  ];
  home.file.".emacs.d/init.el".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/init.el";
  home.file.".emacs.d/doom-cafe-theme.el".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/doom-cafe-theme.el";
  home.file.".emacs.d/lisp".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/lisp";
  home.file.".emacs.d/data".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/data";
}
