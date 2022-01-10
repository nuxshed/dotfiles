{ config, pkgs, libs, ... }:
{
  services.dunst = {
    enable = true;
    settings = {
      global = {
        padding = 12;
        horizontal_padding = 14;

        markup = "full";
        geometry = "0x5-7+30";
        word_wrap = "yes";
        show_indicators = false;
        format = "<b>%s</b>\n%b";
        font = "JetBrainsMono Nerd Font 9";

        icon_position = "left";
        max_icon_size = 80;

        frame_width = "0";
        frame_color = "#2c363c";

        progress_bar = true;
        progress_bar_height = 10;

        separator_height = 3;
        separator_color = "frame";
      };

      urgency_low = {
        background = "#f0edec";
        foreground = "#2c363c";
        highlight = "#2c363c";
        timeout = 10;
      };

      urgency_normal = {
        background = "#f0edec";
        foreground = "#2c363c";
        highlight = "#2c363c";
        timeout = 10;
      };

      urgency_critical = {
        background = "#f0edec";
        foreground = "#2c363c";
        highlight = "#2c363c";
        frame_color = "#A8334C";
        timeout = 0;
      };
    };
  };
}
