{ config, modulesPath, pkgs, cardano-configurations, ... }:
{
  imports = [ "${modulesPath}/virtualisation/qemu-vm.nix" ];

  virtualisation = {
    memorySize = 8192;
    diskSize = 100000;
    forwardPorts = [
      # SSH
      { from = "host"; host.port = 2222; guest.port = 22; }
      # Ogmios
      { from = "host"; host.port = 1337; guest.port = 1337; }
      # Kupo
      { from = "host"; host.port = 1442; guest.port = 1442; }
    ];
  };

  # WARNING: root access with empty password for debugging via console and ssh
  networking.firewall.enable = false;
  services.getty.autologinUser = "root";
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "yes";
  users.extraUsers.root.password = "";
  users.mutableUsers = false;

  # services

  services.cardano-node = {
    enable = true;
    systemdSocketActivation = true;
    nodeConfigFile = "${cardano-configurations}/network/sanchonet/cardano-node/config.json";
    topology = "${cardano-configurations}/network/sanchonet/cardano-node/topology.json";
  };

  services.ogmios = {
    enable = true;
    host = "0.0.0.0";
    nodeSocket = "/var/run/cardano-node/node.socket";
  };

  services.kupo = {
    enable = true;
    host = "0.0.0.0";
    user = "kupo";
    group = "kupo";
    nodeConfig = "${cardano-configurations}/network/sanchonet/cardano-node/config.json";
    nodeSocket = "/var/run/cardano-node/node.socket";
  };
}
