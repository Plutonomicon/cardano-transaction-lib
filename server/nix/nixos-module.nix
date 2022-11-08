{ config, lib, pkgs, ... }:

let
  cfg = config.services.ctl-server;
in
{

  options.services.ctl-server = {
    enable = lib.mkEnableOption "ctl-server";

    user = lib.mkOption {
      description = "User to run ctl-server service as.";
      type = lib.types.str;
      default = "ctl-server";
    };

    group = lib.mkOption {
      description = "Group to run ctl-server service as.";
      type = lib.types.str;
      default = "ctl-server";
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.group;
    };

    users.groups.${cfg.group} = { };

    systemd.services.ctl-server = {
      enable = true;
      after = [ "cardano-node.service" ];
      wantedBy = [ "multi-user.target" ];

      script = lib.escapeShellArgs (lib.concatLists [
        [ pkgs.ctl-server ]
      ]);

      serviceConfig = {
        User = cfg.user;
        Group = cfg.group;
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
        SystemCallFilter = [ "~@cpu-emulation @debug @keyring @mount @obsolete @privileged @setuid @resources" ];
        MemoryDenyWriteExecute = true;
      };
    };
  };
}
