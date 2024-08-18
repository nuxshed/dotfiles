# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).

{ config, pkgs, self, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ../../modules/system
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking = {
    hostName = "zephyrus";
    networkmanager.enable = true;
  };

  services.usbmuxd = {
    enable = true;
    package = pkgs.usbmuxd2;
  };

  hardware.bluetooth.enable = true;
  hardware.pulseaudio.enable = true;

  services.supergfxd.enable = true;
  systemd.services.supergfxd.path = [pkgs.pciutils ];
  services.asusd = {enable = true; enableUserService = true; };

  time.timeZone = "Asia/Kolkata";

  console = {
    font = "Lat2-Terminus16";
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableGlobalCompInit = false;
  };

  environment.binsh = "${pkgs.dash}/bin/dash";

  users.users.nuxsh = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    shell = pkgs.zsh;
  };

  environment.systemPackages = with pkgs; [
    coreutils
    gcc
    usbutils
    vim
    git
    maim
    xclip
  ];

  programs.nix-ld.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  
  nix = {
    package = pkgs.nix;
    settings.trusted-users = [ "root" "advait" ];
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  system.stateVersion = "24.11";

}

