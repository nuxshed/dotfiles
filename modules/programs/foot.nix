{ config, pkgs, libs, ... }:
{
  programs.foot = {
    enable = true;
    settings = {
      main = {
        term = "xterm-256color";
        font = "JetBrainsMono Nerd Font:size=8";
      };
      mouse = {
        hide-when-typing = "yes";
      };
    };
  };
}
