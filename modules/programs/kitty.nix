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

      # gruvbox material --------------------------------------------
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

      # tokyonight --------------------------------------------------

      # background = "#24283b";
      # foreground = "#c0caf5";
      # selection_background = "#364A82";
      # selection_foreground = "#c0caf5";
      # url_color = "#73daca";
      # cursor = "#c0caf5";
      #
      # active_tab_background = "#7aa2f7";
      # active_tab_foreground = "#1f2335";
      # inactive_tab_background = "#292e42";
      # inactive_tab_foreground = "#545c7e";
      #
      # color0 = "#1D202F";
      # color1 = "#f7768e";
      # color2 = "#9ece6a";
      # color3 = "#e0af68";
      # color4 = "#7aa2f7";
      # color5 = "#bb9af7";
      # color6 = "#7dcfff";
      # color7 = "#a9b1d6";
      # color8 = "#414868";
      # color9 = "#f7768e";
      # color10 = "#9ece6a";
      # color11 = "#e0af68";
      # color12 = "#7aa2f7";
      # color13 = "#bb9af7";
      # color14 = "#7dcfff";
      # color15 = "#c0caf5";
      # color16 = "#ff9e64";
      # color17 = "#db4b4b";


      # whiteout ----------------------------------------------------
      # background = "#f7f7f7";
      # foreground = "#464646";
      # selection_background = "#464646";
      # selection_foreground = "#f7f7f7";
      # url_color = "#525252";
      # cursor = "#cccccc";
      # active_border_color = "#ababab";
      # inactive_border_color = "#e3e3e3";
      # active_tab_background = "#e3e3e3";
      # active_tab_foreground = "#555555";
      # inactive_tab_background = "#f7f7f7";
      # inactive_tab_foreground = "#999999";
      # tab_bar_background = "#f7f7f7";
      #
      # color0 = "#f7f7f7";
      # color1 = "#7c7c7c";
      # color2 = "#8e8e8e";
      # color3 = "#a0a0a0";
      # color4 = "#686868";
      # color5 = "#747474";
      # color6 = "#868686";
      # color7 = "#464646";
      # color8 = "#ababab";
      # color9 = "#7c7c7c";
      # color10 = "#8e8e8e";
      # color11 = "#a0a0a0";
      # color12 = "#686868";
      # color13 = "#747474";
      # color14 = "#868686";
      # color15 = "#000000";
      # color16 = "#999999";
      # color17 = "#5e5e5e";
      # color18 = "#e3e3e3";
      # color19 = "#b9b9b9";
      # color20 = "#525252";
      # color21 = "#252525";

      # onedark -----------------------------------------------------
      # background = "#282c34";
      # foreground = "#abb2bf";
      # selection_background = "#abb2bf";
      # selection_foreground = "#282c34";
      # url_color = "#565c64";
      # cursor = "#abb2bf";
      # active_border_color = "#545862";
      # inactive_border_color = "#353b45";
      # active_tab_background = "#282c34";
      # active_tab_foreground = "#abb2bf";
      # inactive_tab_background = "#1b1d23";
      # inactive_tab_foreground = "#565c64";
      # tab_bar_background = "#282c34";
      #
      # color0 = "#282c34";
      # color1 = "#e06c75";
      # color2 = "#98c379";
      # color3 = "#e5c07b";
      # color4 = "#61afef";
      # color5 = "#c678dd";
      # color6 = "#56b6c2";
      # color7 = "#abb2bf";
      # color8 = "#5c6370";
      # color9 = "#d19a66";
      # color10 = "#353b45";
      # color11 = "#3e4451";
      # color12 = "#565c64";
      # color13 = "#b6bdca";
      # color14 = "#be5046";
      # color15 = "#c8ccd4";
    };
  };
}
