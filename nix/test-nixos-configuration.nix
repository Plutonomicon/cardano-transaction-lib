{ config, modulesPath, pkgs, cardano-configurations, ogmios, kupo, ... }:
{
  imports = [ "${modulesPath}/virtualisation/qemu-vm.nix" ];

  virtualisation = {
    memorySize = 8192;
    diskSize = 100000;
    restrictNetwork = false;
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

  environment.systemPackages = with pkgs; [
    lsof
  ];

  # services

  services.cardano-node = {
    enable = true;
    hostAddr = "0.0.0.0";
    socketPath = "/var/run/cardano-node/node.socket";
    systemdSocketActivation = false;
    nodeConfigFile = "${cardano-configurations}/network/preview/cardano-node/config.json";
    topology = "${cardano-configurations}/network/preview/cardano-node/topology.json";
  };

  services.ogmios = {
    enable = true;
    package = ogmios;
    host = "0.0.0.0";
    user = "cardano-node";
    group = "cardano-node";
    nodeSocketPath = "/var/run/cardano-node/node.socket";
    nodeConfigPath = "${cardano-configurations}/network/preview/cardano-node/config.json";
  };

  services.kupo = {
    enable = true;
    package = kupo;
    user = "cardano-node";
    group = "cardano-node";
    host = "0.0.0.0";
    nodeSocketPath = "/var/run/cardano-node/node.socket";
    nodeConfigPath = "${cardano-configurations}/network/preview/cardano-node/config.json";
  };
}
