{ config, pkgs, libs, ... }:
{
  programs.git = {
    enable = true;
    userName = "nuxsh";
    userEmail = "nuxshed@gmail.com";
    signing = {
      key = "76805348FB8DFB8C";
      signByDefault = true;
    };
    ignores = [
      "*.o" ".DS_Store"
    ];
  };
}
