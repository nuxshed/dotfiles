{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [ clojure leiningen jre8 ];
}
