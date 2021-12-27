{ config, pkgs, libs, ... }:
{
  home.packages = [ pkgs.nodejs_latest ];
  home.sessionVariables = {
    NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/config";
    NPM_CONFIG_CACHE = "$XDG_CACHE_HOME/npm";
    NPM_CONFIG_TMP = "$XDG_RUNTIME_DIR/npm";
    NPM_CONFIG_PREFIX = "$XDG_CACHE_HOME/npm";
    NODE_REPL_HISTORY = "$XDG_CACHE_HOME/node/repl_history";
  };
}
