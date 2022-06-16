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
      ((emacsPackagesFor emacsGit).emacsWithPackages (epkgs: [
        epkgs.vterm
      ]))
    ];
    home.file.".emacs.d/init.el".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/init.el";
    home.file.".emacs.d/early-init.el".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/early-init.el";
    home.file.".emacs.d/themes".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/themes";
    home.file.".emacs.d/lisp".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/lisp";
    home.file.".emacs.d/.dir-locals.el".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs/.dir-locals.el";
  };
}
