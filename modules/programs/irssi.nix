{ config, pkgs, lib, ... }:

with lib;
let cfg = config.modules.programs.irssi;
in
{

  options.modules.programs.irssi = {
    enable = mkEnableOption "irssi";
  };

  config = mkIf cfg.enable {
    programs.irssi = {
      enable = true;
      networks = {
        liberachat = {
          nick = "nuxsh";
          server = {
            address = "irc.libera.chat";
            port = 6697;
            autoConnect = true;
          };
          channels = {
            nixos.autoJoin = true;
          };
        };
      };
    };
  };
}
