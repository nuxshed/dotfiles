{ config, pkgs, lib, ... }:

with lib;
let cfg = config.modules.editors.neovim;
in
{
  options.modules.editors.neovim = {
    enable = mkEnableOption "neovim";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      neovim-nightly
      stylua
      shellcheck
      black
      python39Packages.isort
    ];
    home.file.".config/nvim".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/nvim";
  };
}
