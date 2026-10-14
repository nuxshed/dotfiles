{ config, pkgs, lib, ... }: {
  programs.git = {
    enable = true;
    signing = {
      key = "CC48F6B0DBE33D55";
      signByDefault = true;
    };
    ignores = [ "*.o" ".DS_Store" ];
    settings = {
      user = {
        name = "nuxsh";
        email = "nuxshed@gmail.com";
      };
      url = {
        "https://github.com/" = { insteadOf = "github:"; };
        "git@github.com:" = {
          insteadOf = "gh:";
          pushInsteadOf = "github:";
        };
      };
    };
  };
}
