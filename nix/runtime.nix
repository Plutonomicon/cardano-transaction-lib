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
  };
  kupo = {
    port = 1442;
    since = "origin";
    match = "*/*"; # matches Shelley addresses only
    tag = "v2.2.0";
    # TODO: Do we want to support connection through ogmios?
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
};
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
