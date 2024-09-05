{ config, pkgs, lib, ... }: {
  programs.alacritty = {
    enable = true;
    settings = {
      window.padding = {
        x = 30;
        y = 10;
      };
      font = {
        size = 10;
        normal = {
          family = "Cartograph CF";
          style = "Regular";
        };
      };
      colors.primary = {
        foreground = "#c6c6c6";
        background = "#222228";
      };
      colors.normal = {
        white = "#c6c6c6";
        red = "#e1c1ee";
        green = "#8ca378";
        yellow = "#cfcf9c";
        blue = "#819cd6";
        magenta = "#b0a2e7";
        black = "#222228";
        cyan = "#616c96";
      };
      colors.bright = {
        white = "#eceff4";
        red = "#e1c1ee";
        green = "#8ca378";
        yellow = "#cfcf9c";
        blue = "#819cd6";
        magenta = "#b0a2e7";
        black = "#515462";
        cyan = "#616c96";
      };
      colors.dim = {
        white = "#727269";
        red = "#e1c1ee";
        green = "#8ca378";
        yellow = "#cfcf9c";
        blue = "#819cd6";
        magenta = "#b0a2e7";
        black = "#222228";
        cyan = "#616c96";
      };
      colors.selection = {
        background = "#616c96";
        text = "#c6c6c6";
      };
    };
  };
}
