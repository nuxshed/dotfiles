{ config, pkgs, libs, ... }:
{
  accounts.email = {
    maildirBasePath = "/home/advait/.mail";
    accounts = {
      Gmail = {
        address = "nuxshed@gmail.com";
        userName = "nuxshed@gmail.com";
        flavor = "gmail.com";
        passwordCommand = "${pkgs.pass}/bin/pass nuxshed@gmail.com";
        primary = true;
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          patterns = [ "*" "[Gmail]*" ]; # "[Gmail]/Sent Mail" ];
        };
        realName = "nuxsh";
        msmtp.enable = true;
      };
    };
  };

  home.packages = with pkgs; [ mu isync ];

  programs = {
    msmtp.enable = true;
    mbsync.enable = true;
  };
}
