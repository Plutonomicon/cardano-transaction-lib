{ inputs, ... }:
rec {
  defaultConfig = final: with final; {
    inherit (inputs) cardano-configurations;
    # { name = "preprod"; magic = 1; }
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
      # the version of the node to use, corresponds to the image version tag,
      # i.e. `"inputoutput/cardano-node:${tag}"`
      tag = "1.35.4";
    };
    ogmios = { port = 1337; };
    postgres = {
      # User-facing port on host machine.
      # Can be set to null in order to not bind postgres port to host.
      # Postgres will always be accessible via `postgres:5432` from
      # containers.
      port = 5432;
      user = "ctl";
      password = "ctl";
      db = "ctl-${network.name}";
    };
    kupo = {
      port = 1442;
      since = "origin";
      match = "*/*"; # matches Shelley addresses onlsy
      tag = "v2.2.0";
      # TODO: Do we want to support connection through ogmios?
    };
    blockfrost = {
      enable = false;
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
      nodeSocketPath = "/ipc/node.socket";
      bindPort = port: "${toString port}:${toString port}";
      defaultServices = with config; {
        cardano-node = {
          service = {
            image = "inputoutput/cardano-node:${node.tag}";
            ports = [ (bindPort node.port) ];
            volumes = [
              "${config.cardano-configurations}/network/${config.network.name}/cardano-node:/config"
              "${config.cardano-configurations}/network/${config.network.name}/genesis:/genesis"
              "${nodeDbVol}:/data"
              "${nodeIpcVol}:/ipc"
            ];
            command = [
              "run"
              "--config"
              "/config/config.json"
              "--database-path"
              "/data/db"
              "--socket-path"
              "${nodeSocketPath}"
              "--topology"
              "/config/topology.json"
            ];
          };
        };
        kupo = {
          service = {
            image = "cardanosolutions/kupo:${kupo.tag}";
            ports = [ (bindPort kupo.port) ];
            volumes = [
              "${config.cardano-configurations}/network/${config.network.name}:/config"
              "${nodeIpcVol}:/ipc"
              "${kupoDbVol}:/kupo-db"
            ];
            command = [
              "--node-config"
              "/config/cardano-node/config.json"
              "--node-socket"
              "${nodeSocketPath}"
              "--since"
              "${kupo.since}"
              "--defer-db-indexes"
              "--match"
              "${"${kupo.match}"}"
              "--host"
              "0.0.0.0"
              "--workdir"
              "kupo-db"
              "--prune-utxo"
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
              ''
            ];
          };
        };
        "postgres-${network.name}" = {
          service = {
            image = "postgres:13";
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
      } // (if config.blockfrost.enable then {
        "db-sync-${network.name}" = {
          build.image = pkgs.lib.mkForce inputs.db-sync.packages.${pkgs.system}.dockerImage;
          service = {
            useHostStore = true;
            environment = {
              NETWORK = network.name;
              POSTGRES_HOST = "postgres-${network.name}";
              POSTGRES_PORT = postgres.port;
              POSTGRES_USER = postgres.user;
              PGPASSWORD = postgres.password;
              # POSTGRES_PASSWORD = postgres.password;
            };
            depends_on = [ "postgres-${network.name}" ];
            volumes = [
              "${nodeIpcVol}:/ipc"
              "${config.cardano-configurations}/network/${config.network.name}/cardano-db-sync:/config"
              "${config.cardano-configurations}/network/${config.network.name}/genesis:/genesis"
            ];
          };
        };
        blockfrost = {
          build.image = pkgs.lib.mkForce inputs.blockfrost.packages.${pkgs.system}.dockerImage;
          service = {
            useHostStore = true;
            ports = [ (bindPort 3000) ];
            environment ={
              BLOCKFROST_CONFIG_SERVER_PORT = 3000;
              BLOCKFROST_CONFIG_SERVER_LISTEN_ADDRESS = "0.0.0.0";
              BLOCKFROST_CONFIG_SERVER_DEBUG= "false";
              BLOCKFROST_CONFIG_DBSYNC_HOST = "postgres-${network.name}:5432";
              BLOCKFROST_CONFIG_DBSYNC_USER = postgres.user;
              BLOCKFROST_CONFIG_DBSYNC_DATABASE = postgres.db;
              BLOCKFROST_CONFIG_DBSYNC_MAX_CONN = "1";
              BLOCKFROST_CONFIG_NETWORK = "preview";
              BLOCKFROST_CONFIG_TOKEN_REGISTRY_URL = "https://tokens.cardano.org";
              PGPASSWORD = postgres.password;
            };
          };
        };
      } else {});
    in
    {
      docker-compose.raw = pkgs.lib.recursiveUpdate
        {
          volumes = {
            "${nodeDbVol}" = { };
            "${nodeIpcVol}" = { };
            "${kupoDbVol}" = { };
          };
        }
        config.extraDockerCompose;
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
