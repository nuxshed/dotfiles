{ config, pkgs, libs, ... }:
{
  home.packages = with pkgs; [
    neovim-nightly
    stylua
    shellcheck
    black
    python39Packages.isort
  ];
  home.file.".config/nvim".source =
  config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/nvim";
}
