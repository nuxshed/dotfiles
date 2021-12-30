{ config, pkgs, libs, ... }:
{
  imports = [ ./colors/gruvbox.nix ];

  programs.kitty = {
    enable = true;
    font = {
      name = "JetBrainsMono Nerd Font 8";
      size = 8;
    };
    settings = {
      cursor_shape = "underline";
      window_padding_width = 10;
      scrollback_lines = 5000;
      tab_bar_style = "fade";
      tab_fade = 1;
      tab_bar_margin_width = 5;
      tab_bar_margin_height = 5;
      allow_remote_control = "yes";
      listen_on = "unix:/tmp/mykitty";
    };
  };
}
