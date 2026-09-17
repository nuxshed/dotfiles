{ config, pkgs, lib, inputs, ... }:
let
  # College resolvers, used only if the server stops pushing dhcp-option DNS.
  collegeDNSFallback = [ "10.4.20.21" "10.4.20.22" ];

  # Domains routed to the college resolvers. Everything else keeps using the
  # normal DHCP resolver, so the tunnel never sees unrelated lookups.
  # The in-addr.arpa entries cover reverse DNS for the subnets pushed on tun0.
  collegeDomains = [ "~iiit.ac.in" "~10.in-addr.arpa" "~36.168.192.in-addr.arpa" ];

  vpnUp = pkgs.writeShellScript "openvpn-college-up" ''
    set -eu
    export PATH=${lib.makeBinPath [ pkgs.systemd ]}:$PATH

    # Prefer whatever the server pushed; fall back to the known campus resolvers.
    dns=""
    for name in ''${!foreign_option_@}; do
      case "''${!name}" in
        "dhcp-option DNS "*) dns="$dns ''${!name#dhcp-option DNS }" ;;
      esac
    done
    [ -n "$dns" ] || dns="${lib.concatStringsSep " " collegeDNSFallback}"

    # Scope those resolvers to tun0 only, and never let it win the default route.
    resolvectl dns "$dev" $dns
    resolvectl domain "$dev" ${
      lib.concatStringsSep " " (map (d: "'${d}'") collegeDomains)
    }
    resolvectl default-route "$dev" false
    resolvectl flush-caches || true
  '';

  vpnDown = pkgs.writeShellScript "openvpn-college-down" ''
    set -eu
    export PATH=${lib.makeBinPath [ pkgs.systemd ]}:$PATH
    resolvectl revert "$dev" || true
    resolvectl flush-caches || true
  '';
in
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
  services.logind.settings.Login.HandleLidSwitch = "lock";
  services.logind.settings.Login.HandleLidSwitchExternalPower = "lock";

  networking.firewall = {
    enable = false;
  };

  # Split DNS. NetworkManager hands resolution to systemd-resolved so DNS can be
  # scoped per interface instead of one global /etc/resolv.conf.
  services.resolved.enable = true;
  networking.networkmanager.dns = "systemd-resolved";

  services.openvpn.servers.college = {
    config = ''
      config /etc/openvpn/college.ovpn

      # The server does not push redirect-gateway, so routing is already split:
      # only the campus subnets land on tun0. Ignore it defensively in case that
      # ever changes on their end.
      pull-filter ignore "redirect-gateway"

      # Take the pushed DNS, but apply it ourselves (scoped) rather than letting
      # it overwrite the global resolver.
      script-security 2
      up ${vpnUp}
      down ${vpnDown}
      down-pre
    '';
    authUserPass = "/etc/openvpn/college-auth.txt";

    autoStart = true;
    # Handled by the up/down scripts above; this would clobber /etc/resolv.conf.
    updateResolvConf = false;
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
  services.displayManager.defaultSession = "hyprland";

  qt.enable = true;

  services.displayManager.ly = {
    enable = true;
  };

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-termfilechooser ];
    # Route file dialogs to the Quickshell picker (bin/qs-filechooser) via termfilechooser.
    config.hyprland = {
      default = [ "hyprland" "gtk" ];
      "org.freedesktop.impl.portal.FileChooser" = [ "termfilechooser" ];
    };
  };

  services.gnome.gnome-keyring.enable = true;

  security.pam.services.ly.enableGnomeKeyring = true;

  services.dbus.packages = [ pkgs.gcr ];

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
    pkgs.qt5.qtgraphicaleffects 
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
    pkgs.libsecret
    pkgs.gcr
    pkgs.seahorse
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
