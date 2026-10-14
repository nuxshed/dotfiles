{ config, pkgs, ... }: {
  networking.networkmanager.enable = true;

  time.timeZone = "Asia/Kolkata";

  console.font = "Lat2-Terminus16";

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

  programs.nix-ld.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # `nh os switch` / `nh os boot` / `nh os test`: builds, shows a package diff, then switches.
  programs.nh = {
    enable = true;
    flake = "/home/nuxsh/dotfiles";
  };

  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    trusted-users = [ "nuxsh" ];
    # emacs-overlay and neovim-nightly builds live here; without it they compile locally.
    extra-substituters = [ "https://nix-community.cachix.org" ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}
