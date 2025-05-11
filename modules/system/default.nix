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

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 80 8080 5173 ];
  };

  virtualisation.docker.enable = true;

  nixpkgs.config.allowUnfree = true;

  services.openvpn.servers = {
    college = {
      config = ''
        config /home/nuxsh/.config/openvpn.conf
      '';
      updateResolvConf = true;
    };
  };

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
    mongodb-tools
    mongosh
  ];

  services.onedrive.enable = true;

  services.mongodb.enable = true;
  services.mongodb.package = pkgs.mongodb-ce;

}
