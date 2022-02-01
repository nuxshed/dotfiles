{ config, pkgs, lib, ... }:

with lib;
let cfg = config.modules.dev.clojure;
in
{
  options.modules.dev.clojure = {
    enable = mkEnableOption "clojure";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ clojure leiningen jre8 ];
  };
}
