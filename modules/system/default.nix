{ config, pkgs, inputs, ... }: {
  imports = [ ./env.nix ./fonts.nix ./xserver.nix inputs.agenix.nixosModules.default ];
  environment.systemPackages = [ inputs.agenix.packages.x86_64-linux.default ];
  services.onedrive.enable = true;

  # secrets
  # agenix = {
  #   enable = true;
  #   secrets = ./secrets/secrets.nix;
  # };
}
