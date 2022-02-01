{ config, pkgs, lib, ... }:

with lib;
let cfg = config.modules.dev.nix;
in
{
  options.modules.dev.nix = {
    enable = mkEnableOption "nix";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      rnix-lsp
    ];
  };
}
