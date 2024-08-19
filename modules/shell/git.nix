{ config, pkgs, libs, ... }: {
  programs.git = {
    enable = true;
    userName = "nuxsh";
    userEmail = "nuxshed@gmail.com";
    signing = {
      key = "CC48F6B0DBE33D55";
      signByDefault = true;
    };
    ignores = [ "*.o" ".DS_Store" ];
    extraConfig = {
      url = {
        "git://github.com/" = { insteadOf = "github:"; };
        "git@github.com:" = {
          insteadOf = "gh:";
          pushInsteadOf = [ "github:" "git://github.com/" ];
        };
      };
    };
  };
}
