{ config, pkgs, lib, ... }:
{
  imports = [ ./kitty ./spotify.nix ];
  home.packages = with pkgs; [ brave font-manager rofi ];

  nixpkgs.config = {
    allowUnfree = true;
  };

  programs.zathura = {
    enable = true;
    options = {
      recolor = true;
      default-bg = "#eaecf2";
      default-fg = "#343b58";
      recolor-darkcolor = "#343b58";
      recolor-lightcolor = "#eaecf2";
      notification-fg = "#343b58";
      notification-bg = "#998f83";
      statusbar-bg = "#343b58";
      statusbar-fg = "#998f83";
      statusbar-home-tilde = true;
    };
  };

}
