{ config, pkgs, libs, ... }:
{
  programs.git = {
    enable = true;
    userName = "nuxsh";
    userEmail = "nuxshed@gmail.com";
    signing = {
      key = "03F336CD4EE53555";
      signByDefault = true;
    };
    ignores = [
      "*.o"
      ".DS_Store"
    ];
    extraConfig = {
      url = {
        "git://github.com/" = {
          insteadOf = "github:";
        };
        "git@github.com:" = {
          insteadOf = "gh:";
          pushInsteadOf = [ "github:" "git://github.com/" ];
        };
      };
    };
  };
}
