{ config, pkgs, inputs, ... }: {
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.supportedFilesystems = [ "ntfs" ];

  services.usbmuxd = {
    enable = true;
    package = pkgs.usbmuxd2;
  };

  hardware.bluetooth.enable = true;

  imports = [ ./env.nix ./fonts.nix ./xserver.nix ];
  environment.systemPackages = with pkgs; [
    coreutils
    gcc
    usbutils
    vim
    git
    maim
    xclip
    clang
    llvm
    clang-tools
  ];

  services.onedrive.enable = true;

}