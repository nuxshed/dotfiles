{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ../../modules/system ];

  networking.hostName = "earth";

  console.keyMap = "uk";

  system.stateVersion = "23.05";
}
