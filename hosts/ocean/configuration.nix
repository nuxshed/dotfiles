{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix

      # Other Modules
      ../../modules/system/env.nix
      ../../modules/system/sound.nix
      ../../modules/system/xorg.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "Asia/Kolkata";

  networking = {
    hostName = "ocean";
    networkmanager.enable = true;
    useDHCP = false;
    interfaces.enp2s0.useDHCP = true;
    interfaces.wlp3s0.useDHCP = true;
  };

  services.avahi = {
    enable = true;
    nssmdns = true;
  };

  hardware.bluetooth.enable = true;

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "uk";
  };

  programs.gnupg.agent.enable = true;

  users.users.advait = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    shell = pkgs.zsh;
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableGlobalCompInit = false;
  };

  environment.systemPackages = with pkgs; [
    coreutils
    gcc
    gnupg
    nix-zsh-completions
    papirus-icon-theme
    pinentry
    python
    usbutils
    vim
    zsh
  ];

  fonts.fonts = with pkgs; [
    noto-fonts
    dejavu_fonts
    (nerdfonts.override { fonts = [ "Agave" "FiraCode" "JetBrainsMono" ]; })
  ];

  nix = {
    package = pkgs.nixUnstable;
    trustedUsers = [ "root" "advait" "@wheel" ];
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
   };

  system.stateVersion = "21.11";
}
