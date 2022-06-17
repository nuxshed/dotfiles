{ config, pkgs, libs, ... }:
let
  mpd-visualizer-fifo = "/tmp/mpd-visualizer-${config.home.username}.fifo";
in
{
  programs.ncmpcpp = {
    enable = true;
    package = pkgs.ncmpcpp.override { visualizerSupport = true; };
    bindings = [
      { key = "j"; command = "scroll_down"; }
      { key = "k"; command = "scroll_up"; }
      { key = "J"; command = [ "select_item" "scroll_down" ]; }
      { key = "K"; command = [ "select_item" "scroll_up" ]; }
    ];
    settings = {
      visualizer_data_source = mpd-visualizer-fifo;
      visualizer_in_stereo = true;
      visualizer_output_name = "Visualizer FIFO";
    };
  };

  services.mpd = {
    enable = true;
    musicDirectory = "/home/advait/Music";
    extraConfig = ''
      audio_output {
        type  "pulse"
        name  "mpd pulse-audio-output"
      }
    '';
  };

  services.mpdris2 = {
    enable = true;
    notifications = true;
  };
}
