{ config, pkgs, libs, ... }:
{
  programs.git = {
    enable = true;
    userName = "nuxsh";
    userEmail = "nuxshed@gmail.com";
    signing = {
      key = "1590AC256028E2D5";
      signByDefault = true;
    };
    ignores = [
      "*.o" ".DS_Store"
    ];
  };
}
