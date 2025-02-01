{ config, pkgs, lib, ... }: {
  programs.alacritty.settings.colors = {
      primary = {
        foreground = "#E1E1E1";
        background = "#151515";
      };
      normal = {
        white = "#E1E1E1";
        red = "#b46958";
        green = "#90A959";
        yellow = "#F4BF75";
        blue = "#BAD7FF";
        magenta = "#AA759F";
        black = "#151515";
        cyan = "#88afa2";
      };
      bright = {
        white = "#E1E1E1";
        red = "#b46958";
        green = "#90A959";
        yellow = "#F4BF75";
        blue = "#BAD7FF";
        magenta = "#AA759F";
        black = "#373737";
        cyan = "#88afa2";
      };
      dim = {
        white = "#373737";
        red = "#b46958";
        green = "#90A959";
        yellow = "#F4BF75";
        blue = "#BAD7FF";
        magenta = "#AA759F";
        black = "#171717";
        cyan = "#88afa2";
      };
      selection = {
        background = "#373737";
        text = "#E1E1E1";
      };
    };
}
