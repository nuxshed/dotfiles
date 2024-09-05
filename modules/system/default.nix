{ config, pkgs, inputs, ... }: {
  imports = [ ./env.nix ./fonts.nix ./xserver.nix ];
  environment.systemPackages = [ inputs.agenix.packages.x86_64-linux.default ];
  services.onedrive.enable = true;
}
