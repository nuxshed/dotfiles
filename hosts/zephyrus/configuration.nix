# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).

{ config, pkgs, self, ... }:

{
  imports = [ ./hardware-configuration.nix ../../modules/system ];

  networking = {
    hostName = "zephyrus";
    networkmanager.enable = true;
  };

  hardware.pulseaudio.enable = false;

  services.supergfxd.enable = true;
  systemd.services.supergfxd.path = [ pkgs.pciutils ];

  services.asusd = {
    enable = true;
    enableUserService = true;
  };

  time.timeZone = "Asia/Kolkata";

  console = { font = "Lat2-Terminus16"; };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableGlobalCompInit = false;
  };

  environment.binsh = "${pkgs.dash}/bin/dash";

  users.users.nuxsh = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "audio" ];
    shell = pkgs.zsh;
  };

  programs.nix-ld.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
  };

  nix = {
    package = pkgs.nix;
    settings.trusted-users = [ "root" "nuxsh" ];
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  system.stateVersion = "24.11";
}
