{ config, pkgs, lib, ... }:

with lib;
let cfg = config.modules.dev.rust;
in
{
  options.modules.dev.rust = {
    enable = mkEnableOption "rust";
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.rustup ];
    home.sessionVariables = {
      RUSTUP_HOME = "$XDG_DATA_HOME/rustup";
      CARGO_HOME = "$XDG_DATA_HOME/cargo";
    };
    home.sessionPath = [ "$CARGO_HOME/bin" ];
  };
}
