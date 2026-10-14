{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ../../modules/system ];

  networking.hostName = "zephyrus";

  services.pulseaudio.enable = false;

  services.supergfxd.enable = true;
  systemd.services.supergfxd.path = [ pkgs.pciutils ];

  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    open = true;
    # Intel Arc renders everything; the RTX 4070 only wakes for `nvidia-offload <cmd>`
    # (Steam: `nvidia-offload %command%`) or a monitor on HDMI/DP-5, which are wired to it.
    prime = {
      offload.enable = true;
      offload.enableOffloadCmd = true;
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };
    # RTD3: power the dGPU off entirely when idle.
    powerManagement.enable = true;
    powerManagement.finegrained = true;
  };

  services.asusd.enable = true;
  # Power profiles (asusctl and the Fn fan key drive these too).
  services.power-profiles-daemon.enable = true;

  users.users.nuxsh.extraGroups = [ "audio" "docker" "dialout" "tty" ];

  programs.nix-ld.libraries = [ pkgs.stdenv.cc.cc ];

  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
  };

  services.postgresql = {
    enable = true;
    enableTCPIP = true;
  };

  # Databases start on demand (`sudo systemctl start mysql postgresql`), not at boot.
  systemd.services.mysql.wantedBy = lib.mkForce [ ];
  systemd.services.postgresql.wantedBy = lib.mkForce [ ];

  # 15G and no swap device. A flake update once let nix fan out to 22 parallel
  # builds on 22 cores each; cc1plus exhausted RAM and the OOM killer took out
  # dbus-broker and user@1000.service, leaving the session without a bus.
  zramSwap.enable = true;

  nix.settings = {
    max-jobs = 4;
    cores = 4;
  };

  system.stateVersion = "26.05";
}
