{ config, pkgs, lib, ... }:

let
  home = config.home.homeDirectory;
  indexDir = "${home}/.cache/spotlight";
  indexDb = "${indexDir}/index.db";

  prunedDirs = [
    ".cache" ".direnv" ".git" ".local/share/Trash" ".local/state"
    ".mozilla" ".npm" ".steam" "node_modules" "result" "target"
  ];

  spotlight-index = pkgs.writeShellApplication {
    name = "spotlight-index";
    runtimeInputs = [ pkgs.fd pkgs.plocate pkgs.coreutils ];
    text = ''
      mkdir -p ${indexDir}
      list=$(mktemp)
      db=$(mktemp)
      trap 'rm -f "$list" "$db"' EXIT

      fd --hidden --absolute-path --no-follow --type f --type d \
        ${lib.concatMapStringsSep " " (d: "--exclude '${d}'") prunedDirs} \
        . "${home}" > "$list" 2>/dev/null || true

      plocate-build -p "$list" "$db"
      mv -f "$db" "${indexDb}"
      chmod 600 "${indexDb}"
    '';
  };
in {
  home.packages = [
    pkgs.plocate
    pkgs.libqalculate
    pkgs.cliphist
    spotlight-index
  ];

  systemd.user.services.spotlight-index = {
    Unit.Description = "Build the spotlight file index";
    Service = {
      Type = "oneshot";
      TimeoutStartSec = "5min";
      Nice = 10;
      IOSchedulingClass = "idle";
      ExecStart = lib.getExe spotlight-index;
    };
  };

  systemd.user.timers.spotlight-index = {
    Unit.Description = "Rebuild the spotlight file index periodically";
    Timer = {
      OnStartupSec = "2min";
      OnUnitActiveSec = "15min";
      Persistent = true;
    };
    Install.WantedBy = [ "timers.target" ];
  };

  systemd.user.services.cliphist = {
    Unit = {
      Description = "Clipboard history for spotlight";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = "${pkgs.wl-clipboard}/bin/wl-paste --type text --watch ${pkgs.cliphist}/bin/cliphist store";
      Restart = "on-failure";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
