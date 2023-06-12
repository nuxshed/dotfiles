{ config, pkgs, lib, ... }:

{
  imports = [ ./colors/hydrangea.nix ];


  config = {
    programs.kitty = {
      enable = true;
      settings = {
        cursor_shape = "underline";
        window_padding_width = "20 30 20 30";
        scrollback_lines = 5000;
        tab_bar_style = "fade";
        tab_fade = 1;
        tab_bar_margin_width = 5;
        tab_bar_margin_height = 5;
        confirm_os_window_close = 0;
        allow_remote_control = "yes";
        listen_on = "unix:/tmp/mykitty";
      };
    };
  };
}
