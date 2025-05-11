{ inputs, config, pkgs, lib, ... }: {
  home.packages = with pkgs; [ inputs.zen-browser.packages."${system}".default firefox font-manager inkscape xdotool xcolor obsidian ];
  imports = [ ./alacritty ];

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
      default-bg = "#141414";
      default-fg = "#c6c6c6";
      recolor-darkcolor = "#c6c6c6";
      recolor-lightcolor = "#141414";
      statusbar-home-tilde = true;
      guioptions = "none";
    };
  };

}
