{ config, modulesPath, pkgs, cardano-configurations, ... }:
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

  cardano = {
    network = "preview";
    node = {
      enable = true;
      socketPath = "/var/run/cardano-node/node.socket";
      configPath = "${cardano-configurations}/network/preview/cardano-node/config.json";
    };
    cli.enable = true;
    ogmios.enable = true;
    kupo.enable = true;
  };
}
