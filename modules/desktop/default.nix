{ inputs, config, pkgs, lib, ... }:
{
  imports = [];
  home.packages = with pkgs; [ dunst libnotify lounge-gtk-theme picom-pijulius papirus-icon-theme redshift slock tint2 xss-lock  ];

  xsession = {
    enable = true;
    initExtra = "~/.fehbg\nxss-lock slock & picom &";
  };

  home.file = {
    ".config/awesome".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/awesome";
    "dotfiles/config/awesome/modules/bling".source = inputs.bling.outPath;
    "dotfiles/config/awesome/modules/rubato".source = inputs.rubato.outPath;
  };

  gtk = {
    enable = true;
    theme.name = "Lounge-night-compact";
    iconTheme.name = "Papirus-Dark";
  };

  home.pointerCursor = {
    name = "capitaine-cursors";
    package = pkgs.capitaine-cursors;
    size = 32;
  };
  home.file = {
    ".config/tint2".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/tint2";
  };
}
