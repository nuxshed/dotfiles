{ config, pkgs, lib, ... }:

with lib;
let cfg = config.modules.dev.lisp;
in
{
  options.modules.dev.lisp = {
    enable = mkEnableOption "lisp";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ sbcl lispPackages.quicklisp ];
  };
}
