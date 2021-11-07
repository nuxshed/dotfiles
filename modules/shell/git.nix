{ config, pkgs, libs, ... }:
{
  programs.git = {
    enable = true;
    userName = "nuxsh";
    userEmail = "nuxshed@gmail.com";
    signing = {
      key = "40C354590537FB6D";
      signByDefault = true;
    };
    ignores = [
      "*.o" ".DS_Store"
    ];
  };
}
