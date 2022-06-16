{ config, pkgs, lib, ... }:

with lib;
let cfg = config.modules.programs.kitty;
in
{
  options.modules.programs.kitty = {
    enable = mkEnableOption "kitty";
  };

  imports = [ ./colors/zenbones.nix ];


  config = mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      font = {
        name = "JetBrainsMono Nerd Font";
        size = 9;
      };
      settings = {
        cursor_shape = "underline";
        window_padding_width = "20 30 20 30";
        scrollback_lines = 5000;
        tab_bar_style = "fade";
        tab_fade = 1;
        tab_bar_margin_width = 5;
        tab_bar_margin_height = 5;
        confirm_os_window_close = -1;
        allow_remote_control = "yes";
        listen_on = "unix:/tmp/mykitty";
      };
    };
  };
}
