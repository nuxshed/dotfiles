{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    lua
    lua52Packages.luarocks
    sumneko-lua-language-server
  ];
}
