{ config, pkgs, libs, ... }:
{
  programs.kitty = {
    enable = true;
    font = {
      name = "JetBrainsMono Nerd Font 8";
      size = 8;
    };
    settings = {
      cursor_shape = "underline";
      window_padding_width = 10;
      tab_bar_style = "fade";
      tab_fade = 1;
      tab_bar_margin_width = 5;
      tab_bar_margin_height = 5;
      scrollback_lines = 5000;
      allow_remote_control = "yes";
      listen_on = "unix:/tmp/mykitty";

      # colorschemes

      # gruvbox material
      background = "#1d2021";
      foreground = "#d4be98";
      selection_background = "#d4be98";
      selection_foreground = "#1d2021";

      cursor = "#a89984";
      cursor_text_color = "background";

      active_tab_background = "#282828";
      active_tab_foreground = "#d4be98";
      active_tab_font_style = "bold";
      inactive_tab_background = "#1d2021";
      inactive_tab_foreground = "#a89984";
      inactive_tab_font_style = "normal";

      color0 = "#1d2021";
      color1 = "#ea6962";
      color2 = "#a9b665";
      color3 = "#e78a4e";
      color4 = "#7daea3";
      color5 = "#d3869b";
      color6 = "#89b482";
      color7 = "#d4be98";
      color8 = "#928374";
      color9 = "#ea6962";
      color10 = "#a9b665";
      color11 = "#d8a657";
      color12 = "#7daea3";
      color13 = "#d3869b";
      color14 = "#89b482";
      color15 = "#d4be98";
    };
  };
}
