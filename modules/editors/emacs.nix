{ config, pkgs, lib, ... }:
with lib;
let cfg = config.modules.editors.emacs;
in
{
  options.modules.editors.emacs = {
    enable = mkEnableOption "emacs";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      ((emacsPackagesNgGen emacsGit).emacsWithPackages (epkgs: [
        epkgs.vterm
      ]))
    ];
    home.file.".emacs.d/init.el".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/init.el";
    home.file.".emacs.d/doom-cafe-theme.el".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/doom-cafe-theme.el";
    home.file.".emacs.d/doom-kurai-theme.el".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/doom-kurai-theme.el";
    home.file.".emacs.d/lisp".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/lisp";
    home.file.".emacs.d/snippets".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/snippets";
  };
}
