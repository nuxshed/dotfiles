# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).

{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostName = "earth";
    networkmanager.enable = true;
  };

  hardware.bluetooth.enable = true;

  time.timeZone = "Asia/Kolkata";

  console = {
    font = "Lat2-Terminus16";
    keyMap = "uk";
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableGlobalCompInit = false;
  };

  environment.binsh = "${pkgs.dash}/bin/dash";

  services.xserver = {
    enable = true;
    layout = "gb";
    xkbOptions = "eurosign:e,caps:escape";
    libinput.enable = true;
    displayManager.gdm.enable = true;
 
    windowManager = {
     awesome = {
       enable = true;
     #  pkg = pkgs.awesome-git;
     };
     berry.enable = true;
     herbstluftwm.enable = true;
    };

  };

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
  ];

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  
  nix = {
    package = pkgs.nixUnstable;
    trustedUsers = [ "root" "advait" ];
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  system.stateVersion = "23.05";

}

