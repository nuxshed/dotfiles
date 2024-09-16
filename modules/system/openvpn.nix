{ config, inputs, pkgs, ... }: 
let
  ovpnConfig = builtins.readFile (config.age.secrets.ovpn.path);
in {
  services.openvpn.servers = {
    wifi = {
      autoStart = false;
      updateResolvConf = true;
      config = ovpnConfig;
    };
  };

  systemd.services.openvpn-wifi = {
    after = [ "agenix-apply.service" ];
  };
}
