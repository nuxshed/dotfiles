{ config, pkgs, libs, ... }:
{
  imports = [ ./bottom.nix ./graphics.nix ./irssi.nix ./kitty ./mail.nix ./zathura.nix ];
  home.packages = with pkgs; [ brave firefox rofi ];
  home.file.".config/rofi".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/rofi";
}
