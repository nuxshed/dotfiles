{ config, pkgs, lib, ... }: {
  imports = [
    ../../modules/desktop
    ../../modules/dev
    ../../modules/editors
    ../../modules/programs
    ../../modules/shell
  ];

  home = {
    username = "nuxsh";
    homeDirectory = "/home/nuxsh";
    stateVersion = "23.05";
  };
}
