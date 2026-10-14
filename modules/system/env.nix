{ config, pkgs, ... }: {
  # Unbraced $HOME on purpose: NixOS rewrites it to pam_env's @{HOME}. "${HOME}" stays a
  # pam_env variable lookup, which is empty at login, so PAM-started daemons (gnome-keyring)
  # got XDG_DATA_HOME=/.local/share and never found ~/.local/share/keyrings.
  environment.sessionVariables = {
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_BIN_HOME = "$HOME/.local/bin";
    XDG_DATA_HOME = "$HOME/.local/share";

    PATH = [ "$HOME/.bin" "$HOME/.local/bin" "$HOME/.node_modules" ];
  };
}
