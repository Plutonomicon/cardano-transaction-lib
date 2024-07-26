{ inputs, ... }:
rec {
  defaultConfig = final: with final; {
    inherit (inputs) cardano-configurations;
    # { name = "preprod"; magic = 1; }
    # { name = "preview"; magic = 2; }
    # { name = "sanchonet"; magic = 4; }
    # { name = "mainnet"; magic = null; }
    # See `doc/development.md` and `doc/runtime.md#changing-network-configurations`
    # for info on how to switch networks.
    network = {
      name = "preview";
      magic = 2; # use `null` for mainnet
    };
    # *All* of these values are optional, and shown with their default
    # values. If you need even more customization, you can use `overideAttrs`
    # to change the values after calling `buildCtlRuntime`
    node = {
      port = 3001;
    };
    ogmios = { port = 1337; };
    postgres = {
      port = 5432;
      user = "ctl";
      password = "ctl";
      db = "ctl-${network.name}";
    };
    kupo = {
      port = 1442;
      since = "origin";
      match = "*/*"; # matches Shelley addresses only
      deferDbIndexes = true; # whether to pass --defer-db-indexes
      pruneUtxo = true; # whether to pass --prune-utxo
      # TODO: Do we want to support connection through ogmios?
    };
    blockfrost = {
      enable = false;
      port = 3000;
    };
    # Additional config that will be included in Arion's `docker-compose.raw`. This
    # corresponds directly to YAML that would be written in a `docker-compose` file,
    # e.g. volumes
    extraDockerCompose = { };
    # Additional services to include in the `docker-compose` config that Arion
    # produces.
    #
    # For a docker image:
    #
    # ```
    #   foo = {
    #     service = {
    #       image = "bar:foo";
    #       command = [
    #         "baz"
    #         "--quux"
    #       ];
    #     };
    #   };
    # ```
    #
    # For a Nix package:
    #
    # ```
    #   foo = {
    #     service = {
    #       useHostStore = true;
    #       command = [
    #         "${pkgs.baz}/bin/baz"
    #         "--quux"
    #       ];
    #     };
    #   };
    #
    # ```
    extraServices = { };
  };

  buildCtlRuntime = pkgs: extraConfig: { ... }:
    let
      inherit (builtins) toString;
      config = with pkgs.lib;
        fix (final:
          recursiveUpdate
            (defaultConfig final)
            (
              if isFunction extraConfig
              then extraConfig final
              else extraConfig
            )
        );
      nodeDbVol = "node-${config.network.name}-db";
      nodeIpcVol = "node-${config.network.name}-ipc";
      kupoDbVol = "kupo-${config.network.name}-db";
      dbSyncStateVol = "db-sync-state-${config.network.name}";
      dbSyncTmpVol = "db-sync-tmp-${config.network.name}";
      nodeSocketPath = "/ipc/node.socket";
      bindPort = port: "${toString port}:${toString port}";
      defaultServices = with config; {
        cardano-node = {
          image.contents = [
            # Note: Required, fixes issue with "Failed to start all.. subscriptions".
            #   Creates the /etc/services file.
            pkgs.iana-etc
            pkgs.coreutils
          ];
          service = {
            useHostStore = true;
            ports = [ (bindPort node.port) ];
            volumes = [
              "${config.cardano-configurations}/network/${config.network.name}/cardano-node:/config"
              "${config.cardano-configurations}/network/${config.network.name}/genesis:/genesis"
              "${nodeDbVol}:/data"
              "${nodeIpcVol}:/ipc"
            ];
            command = [
              "${pkgs.bash}/bin/sh"
              "-c"
              ''
                ${inputs.cardano-node.packages."${pkgs.system}".cardano-node}/bin/cardano-node run \
                  --config /config/config.json \
                  --database-path /data/db \
                  --socket-path "${nodeSocketPath}" \
                  --topology /config/topology.json
              ''
            ];
          };
        };
        kupo = {
          service = {
            useHostStore = true;
            ports = [ (bindPort kupo.port) ];
            volumes = [
              "${config.cardano-configurations}/network/${config.network.name}:/config"
              "${nodeIpcVol}:/ipc"
              "${kupoDbVol}:/kupo-db"
            ];
            command = [
              "${pkgs.bash}/bin/sh"
              "-c"
              ''
                ${pkgs.kupo}/bin/kupo \
                  --node-config /config/cardano-node/config.json \
                  --node-socket "${nodeSocketPath}" \
                  --since "${kupo.since}" \
                  --match "${kupo.match}" \
                  --host "0.0.0.0" \
                  --workdir kupo-db ${pkgs.lib.strings.optionalString kupo.pruneUtxo "--prune-utxo"} ${pkgs.lib.strings.optionalString kupo.deferDbIndexes "--defer-db-indexes"}
              ''
            ];
          };
        };
        ogmios = {
          service = {
            useHostStore = true;
            ports = [ (bindPort ogmios.port) ];
            volumes = [
              "${config.cardano-configurations}/network/${config.network.name}:/config"
              "${nodeIpcVol}:/ipc"
            ];
            command = [
              "${pkgs.bash}/bin/sh"
              "-c"
              ''
                ${pkgs.ogmios}/bin/ogmios \
                  --host ogmios \
                  --port ${toString ogmios.port} \
                  --node-socket /ipc/node.socket \
                  --node-config /config/cardano-node/config.json
                  --include-transaction-cbor
              ''
            ];
          };
        };
      } // (if config.blockfrost.enable then {
        # TODO do not directly use Docker images
        # Arion allow to run NixOS in containers, explore this direction
        "postgres-${network.name}" = {
          service = {
            image = "postgres:15.2";
            ports =
              if postgres.port == null
              then [ ]
              else [ "${toString postgres.port}:5432" ];
            environment = {
              POSTGRES_USER = "${postgres.user}";
              POSTGRES_PASSWORD = "${postgres.password}";
              POSTGRES_DB = "${postgres.db}";
            };
          };
        };
        "db-sync-${network.name}".service = {
          useHostStore = true;
          depends_on = [ "postgres-${network.name}" ];
          volumes = [
            "${dbSyncStateVol}:/state"
            "${config.cardano-configurations}/network/${config.network.name}:/config"
            "${nodeIpcVol}:/ipc"
            "${dbSyncTmpVol}:/tmp"
          ];
          environment = {
            PGPASSFILE = (pkgs.writeText "PGPASSFILE"
              "postgres-${network.name}:${toString postgres.port}:${postgres.db}:${postgres.user}:${postgres.password}").outPath;
            PGPASSWORD = postgres.password;
          };
          command =
            let
              start-db-sync = pkgs.writeShellApplication {
                name = "start-db-sync";
                runtimeInputs = with pkgs; [ bash busybox cardano-db-sync postgresql ];
                text = ''
                  mkdir -p /bin
                  ln -sf "${pkgs.bash}/bin/sh" /bin/sh
                  cardano-db-sync \
                    --config /config/cardano-db-sync/config.json \
                    --socket-path /ipc/node.socket \
                    --state-dir /state \
                    --schema-dir ${inputs.db-sync}/schema
                '';
              };
            in
            [ "${start-db-sync}/bin/start-db-sync" ];
        };
        blockfrost.service = {
          useHostStore = true;
          ports = [ (bindPort blockfrost.port) ];
          depends_on = [ "db-sync-${network.name}" "postgres-${network.name}" ];
          environment = {
            BLOCKFROST_CONFIG_SERVER_PORT = toString blockfrost.port;
            BLOCKFROST_CONFIG_SERVER_LISTEN_ADDRESS = "0.0.0.0";
            BLOCKFROST_CONFIG_SERVER_DEBUG = "false";
            # FIXME after https://github.com/blockfrost/blockfrost-backend-ryo/issues/88
            BLOCKFROST_CONFIG_DBSYNC_HOST = "postgres-${network.name}";
            BLOCKFROST_CONFIG_DBSYNC_USER = postgres.user;
            BLOCKFROST_CONFIG_DBSYNC_DATABASE = postgres.db;
            BLOCKFROST_CONFIG_DBSYNC_MAX_CONN = "1";
            BLOCKFROST_CONFIG_NETWORK = "${network.name}";
            BLOCKFROST_CONFIG_TOKEN_REGISTRY_URL = "https://tokens.cardano.org";
            PGPASSWORD = postgres.password;
          };
          command = [ "${pkgs.blockfrost-backend-ryo}/bin/blockfrost-backend-ryo" ];
        };
      } else { });
    in
    {
      project.name = "ctl-runtime";
      docker-compose = {
        raw = config.extraDockerCompose;
        volumes = {
          "${nodeDbVol}" = { };
          "${nodeIpcVol}" = { };
          "${kupoDbVol}" = { };
        } // pkgs.lib.optionalAttrs config.blockfrost.enable {
          "${dbSyncStateVol}" = { };
          "${dbSyncTmpVol}" = { };
        };
      };
      services = pkgs.lib.recursiveUpdate defaultServices config.extraServices;
    };

  # Makes a set compatible with flake `apps` to launch all runtime services
  launchCtlRuntime = pkgs: config:
    let
      binPath = "ctl-runtime";
      prebuilt = (pkgs.arion.build {
        inherit pkgs;
        modules = [ (buildCtlRuntime pkgs config) ];
      }).outPath;
      script = pkgs.writeShellApplication {
        name = binPath;
        runtimeInputs = [ pkgs.arion pkgs.docker ];
        text =
          ''
            ${pkgs.arion}/bin/arion --prebuilt-file ${prebuilt} up
          '';
      };
    in
    {
      type = "app";
      program = "${script}/bin/${binPath}";
    };

}
