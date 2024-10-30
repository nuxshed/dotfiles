{ config, pkgs, lib, ... }: {
  programs.alacritty.colors = {
      primary = {
        foreground = "#d7d5d1";
        background = "#000000";
      };
      normal = {
        white = "#bbbbbb";
        red = "#ffdddd";
        green = "#d7d5d1";
        yellow = "#d7d5d1";
        blue = "#d7d5d1";
        magenta = "#d7d5d1";
        black = "#262626";
        cyan = "#d7d5d1";
      };
      bright = {
        white = "#e7e5e3";
        red = "#ffdddd";
        green = "#d7d5d1";
        yellow = "#d7d5d1";
        blue = "#d7d5d1";
        magenta = "#d7d5d1";
        black = "#050505";
        cyan = "#d7d5d1";
      };
      dim = {
        white = "#444444";
        red = "#ffdddd";
        green = "#d7d5d1";
        yellow = "#d7d5d1";
        blue = "#d7d5d1";
        magenta = "#d7d5d1";
        black = "#262626";
        cyan = "#d7d5d1";
      };
      selection = {
        background = "#050505";
        text = "#d7d5d1";
      };
    };
  };
}
