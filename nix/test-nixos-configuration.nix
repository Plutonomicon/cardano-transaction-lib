{ config, modulesPath, pkgs, cardano-configurations, ... }:
{
  imports = [ "${modulesPath}/virtualisation/qemu-vm.nix" ];

  virtualisation = {
    memorySize = 8192;
    diskSize = 100000;
    forwardPorts = [
      { from = "host"; host.port = 2222; guest.port = 22; }
      { from = "host"; host.port = 1337; guest.port = 1337; }
      { from = "host"; host.port = 8081; guest.port = 8081; }
      { from = "host"; host.port = 9999; guest.port = 9999; }
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

  services.postgresql.enable = true;

  services.cardano-node = {
    enable = true;
    systemdSocketActivation = true;
    nodeConfigFile = "${cardano-configurations}/network/mainnet/cardano-node/config.json";
    topology = "${cardano-configurations}/network/mainnet/cardano-node/topology.json";
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
    nodeConfig = "${cardano-configurations}/network/mainnet/cardano-node/config.json";
    nodeSocket = "/var/run/cardano-node/node.socket";
  };

  services.ogmios-datum-cache = {
    enable = true;
    host = "0.0.0.0";
    useLatest = true;
    blockSlot = 5854109;
    blockHash = "85366c607a9777b887733de621aa2008aec9db4f3e6a114fb90ec2909bc06f14";
    blockFilter = builtins.toJSON { const = true; };
  };
}
