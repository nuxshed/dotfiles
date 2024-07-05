{ config, pkgs, lib, ... }:
{
  programs.alacritty = {
    enable = true;
    settings = {
      window.padding = { x = 30; y = 30;};
      font.size = 8;
      colors.primary = {
        foreground = "#343b58";
        background = "#eaecf2";
      };
      colors.normal =  {
        white = "#eaecf2";
        red = "#8c4351";
        green = "#33635c";
        yellow = "#965027";
        blue = "#34548a";
        magenta = "#5a4a78";
        black = "#343b58";
        cyan = "#166775";
      };
      colors.bright =  {
        white = "#eaecf2";
        red = "#8c4351";
        green = "#33635c";
        yellow = "#965027";
        blue = "#34548a";
        magenta = "#5a4a78";
        black = "#343b58";
        cyan = "#166775";
      };
      colors.dim =  {
        white = "#eaecf2";
        red = "#8c4351";
        green = "#33635c";
        yellow = "#965027";
        blue = "#34548a";
        magenta = "#5a4a78";
        black = "#343b58";
        cyan = "#166775";
      };
      colors.selection = {background = "#cbd9e3"; text = "#343b58";};
    };
  };
}
