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

  services.pulseaudio.enable = false;

  services.supergfxd.enable = true;
  systemd.services.supergfxd.path = [ pkgs.pciutils ];

  hardware.graphics.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.open = true;

  services.asusd = {
    enable = true;
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
    extraGroups = [ "wheel" "networkmanager" "audio" "docker" "dialout" "tty" ];
    shell = pkgs.zsh;
  };

  programs.nix-ld = {
    enable = true;
    libraries = with pkgs; [ stdenv.cc.cc ];
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
  };

  services.postgresql = {
    enable = true;
    enableTCPIP = true;
  };

  # 15G and no swap device. A flake update once let nix fan out to 22 parallel
  # builds on 22 cores each; cc1plus exhausted RAM and the OOM killer took out
  # dbus-broker and user@1000.service, leaving the session without a bus.
  zramSwap.enable = true;

  nix = {
    package = pkgs.nix;
    settings = {
      trusted-users = [ "root" "nuxsh" ];
      max-jobs = 4;
      cores = 4;
    };
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  system.stateVersion = "26.05";
}
