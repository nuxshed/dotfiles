{ config, pkgs, lib, ... }:
{
  home.packages = with pkgs; [ vivaldi font-manager rofi ];

  nixpkgs.config = {
    allowUnfree = true;
  };

  home.file.".config/rofi".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/rofi";
  home.file.".config/wezterm".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/wezterm";

  programs.zathura = {
    enable = true;
    options = {
      recolor = true;
      default-bg = "#eaecf2";
      default-fg = "#343b58";
      recolor-darkcolor = "#343b58";
      recolor-lightcolor = "#eaecf2";
      statusbar-home-tilde = true;
      guioptions = "none";
    };
  };

}
