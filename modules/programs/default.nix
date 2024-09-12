{ config, pkgs, lib, ... }: {
  home.packages = with pkgs; [ firefox font-manager heroic rofi inkscape xdotool ];
  imports = [ ./alacritty.nix ];

  nixpkgs.config = { allowUnfree = true; };

  home.file.".config/rofi".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/rofi";
  home.file.".config/wezterm".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/wezterm";
  home.file.".mozilla/firefox/oq8rnh56.default/chrome".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/config/firefox";

  programs.zathura = {
    enable = true;
    options = {
      recolor = true;
      default-bg = "#000000";
      default-fg = "#c6c6c6";
      recolor-darkcolor = "#c6c6c6";
      recolor-lightcolor = "#000000";
      statusbar-home-tilde = true;
      guioptions = "none";
    };
  };

}
