{ config, pkgs, lib, ... }:
{
  home.packages = with pkgs; [
    lua luarocks stylua direnv
    clojure clojure-lsp clj-kondo leiningen jre8
    nixfmt llvmPackages.bintools rustup python3
  ];
  home.file.".config/clj-kondo/config.edn".text = ''
  {:ignore [:unresolved-symbol :unresolved-namespace :unused-value]}
  '';
}
