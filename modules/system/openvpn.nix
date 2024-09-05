{ config, inputs, pkgs, ... }: {
  services.openvpn.servers = {
    wifi = {
      autoStart = false;
      updateResolvConf = true;
    };
  };
}
