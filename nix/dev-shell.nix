{ pkgs
, system
, inputs
, nodeModules
, easy-ps
, nodejs
, compiler ? easy-ps.purs-0_14_5
, ...
}:

with inputs;

let
  ogmios-dc = (
    # One of ODC's dependencies is marked as broken on the stable branch
    # We could just override that one package from unstable, but it's more
    # convenient to just use unstable to build the package
    import nixpkgs-unstable { inherit system; }
  ).haskellPackages.callPackage ogmios-datum-cache
    { };
in
pkgs.mkShell {
  buildInputs = with easy-ps; [
    ogmios.packages.${system}."ogmios:exe:ogmios"
    ogmios-dc
    cardano-node-exe.packages.${system}.cardano-cli
    compiler
    spago
    purs-tidy
    purescript-language-server
    pscid
    spago2nix
    pkgs.nodePackages.node2nix
    nodejs
    pkgs.nixpkgs-fmt
  ];

  shellHook = ''
    __ln-node-modules () {
      local modules=./node_modules
      if test -L "$modules"; then
        rm "$modules";
      elif test -e "$modules"; then
        echo 'refusing to overwrite existing (non-symlinked) `node_modules`'
        exit 1
      fi

      ln -s ${nodeModules}/lib/node_modules "$modules"
    }

    __ln-testnet-config () {
      local cfgdir=./.node-cfg
      if test -e "$cfgdir"; then
        rm -r "$cfgdir"
      fi

      mkdir -p "$cfgdir"/testnet/{config,genesis}

      ln -s ${cardano-configurations}/network/testnet/cardano-node/config.json \
        "$cfgdir"/testnet/config/config.json
      ln -s ${cardano-configurations}/network/testnet/genesis/byron.json \
        "$cfgdir"/testnet/genesis/byron.json
      ln -s ${cardano-configurations}/network/testnet/genesis/shelley.json \
        "$cfgdir"/testnet/genesis/shelley.json
    }

    __ln-node-modules
    __ln-testnet-config

    export NODE_PATH="$PWD/node_modules:$NODE_PATH"
    export PATH="${nodeModules}/bin:$PATH"
    export CARDANO_NODE_SOCKET_PATH="$PWD"/.node/socket/node.socket
    export CARDANO_NODE_CONFIG="$PWD"/.node-cfg/testnet/config/config.json

  '';
}
