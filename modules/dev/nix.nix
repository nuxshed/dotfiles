{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    rnix-lsp
  ];
}
