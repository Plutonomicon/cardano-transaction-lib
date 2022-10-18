{ config, lib, pkgs, ... }:
let
  cfg = config.services.ctl-server;
in
with lib; {
  options.services.ctl-server = with types; {
    enable = mkEnableOption "ctl-server";
    package = mkOption {
      description = "The ctl-server package to use.";
      type = package;
      default = pkgs.ctl-server;
    };
    port = mkOption {
      type = port;
      description = "TCP port to listen on.";
      default = 8081;
    };
    user = mkOption {
      description = "User to run ctl-server as.";
      type = str;
      default = "ctl-server";
    };
    group = mkOption {
      description = "Group to run ctl-server service as.";
      type = str;
      default = "ctl-server";
    };
    configurePostgresql = mkOption {
      description = "Whether to configure postgresql service, create user and databse.";
      type = bool;
      default = true;
    };
    databaseName = mkOption {
      description = "Database to create if configurePostgresql is true.";
      type = str;
      default = "ctl-server";
    };
  };

  config = mkIf cfg.enable {
    users.users.ctl-server = mkIf (cfg.user == "ctl-server") {
      isSystemUser = true;
      group = cfg.group;
      extraGroups = [ "cardano-node" ];
    };
    users.groups.ctl-server = mkIf (cfg.group == "ctl-server") { };

    systemd.services.ctl-server = {
      enable = true;
      wantedBy = [ "multi-user.target" ];
      after = [
        "network.target"
        "postgresql.service"
        "cardano-node.service"
        "ogmios.service"
      ];
      serviceConfig = {
        User = cfg.user;
        Group = cfg.group;
        StateDirectory = "ctl-server";
        ExecStart = "${cfg.package}/bin/ctl-server --port ${toString cfg.port}";

        # Security
        UMask = "0077";
        AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
        CapabilityBoundingSet = [ "CAP_NET_BIND_SERVICE" ];
        ProcSubset = "pid";
        ProtectProc = "invisible";
        NoNewPrivileges = true;
        DevicePolicy = "closed";
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        PrivateUsers = true;
        ProtectHostname = true;
        ProtectClock = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" ];
        RestrictNamespaces = true;
        LockPersonality = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        RemoveIPC = true;
        PrivateMounts = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = [
          "~@cpu-emulation @debug @keyring @mount @obsolete @privileged @setuid @resources"
        ];
        MemoryDenyWriteExecute = true;
      };
    };
  };
}
