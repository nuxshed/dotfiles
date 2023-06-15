{ config, pkgs, lib, ... }:
{
  home.packages = with pkgs; [
    lua luarocks stylua
    clojure leiningen jre8
    nixfmt rnix-lsp
  ];
}
