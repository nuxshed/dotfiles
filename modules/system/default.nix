{ config, pkgs, inputs, ... }: 
{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.supportedFilesystems = [ "ntfs" ];

  services.usbmuxd = {
    enable = true;
    package = pkgs.usbmuxd2;
  };

  hardware.bluetooth.enable = true;
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  services.logind.settings.Login.HandlePowerKey = "ignore";

  networking.firewall = {
    enable = false;
  };

  services.openvpn.servers.college = {
    config = "config /etc/openvpn/college.ovpn";
    authUserPass = "/etc/openvpn/college-auth.txt";

    autoStart = false;
    updateResolvConf = true;
  };

  nixpkgs.overlays = [
    (final: prev: {
      openldap = prev.openldap.overrideAttrs (old: {
        doCheck = false;
      });
    })
  ];

  services.avahi.enable = true;
  services.avahi.nssmdns = true;

  programs.steam.enable = true;

  virtualisation.docker.enable = true;

  programs.hyprland.enable = true;
  services.displayManager.defaultSession = "start-hyprland";

  qt.enable = true;

  services.displayManager.ly = {
    enable = true;
  };

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  xdg.portal.enable = true;

  nixpkgs.config.allowUnfree = true;

  imports = [ ./env.nix ./fonts.nix ./xserver.nix ];

  environment.systemPackages = [
    pkgs.coreutils
    pkgs.gcc
    pkgs.usbutils
    pkgs.vim
    pkgs.git
    pkgs.maim
    pkgs.xclip
    pkgs.clang
    pkgs.llvm
    pkgs.clang-tools
    pkgs.qt6Packages.qt5compat
    pkgs.libsForQt5.qt5.qtgraphicaleffects 
    pkgs.kdePackages.qtbase 
    pkgs.kdePackages.qtdeclarative 
    pkgs.kdePackages.wayland 
    pkgs.kdePackages.wayland-protocols 
    inputs.quickshell.packages.x86_64-linux.default
    pkgs.libxkbcommon
    pkgs.xdg-desktop-portal-hyprland
    pkgs.qt5.qtwayland
    pkgs.qt6.qtwayland
    pkgs.uxplay
    pkgs.avahi
    pkgs.avahi-compat
    pkgs.lutris
    pkgs.heroic
    (pkgs.appimage-run.override {
    extraPkgs = pkgs: with pkgs; [
      libGL
      libglvnd
      vulkan-loader
      mesa
    ];
  })
  ];
}
