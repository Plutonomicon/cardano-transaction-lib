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

pkgs.mkShell {
  buildInputs = with easy-ps; [
    ogmios.packages.${system}."ogmios:exe:ogmios"
    cardano-node.packages.${system}.cardano-cli
    compiler
    spago
    purescript-language-server
    purty
    pscid
    spago2nix
    pkgs.nodePackages.node2nix
    nodejs
  ];

  shellHook = ''
    __ln-node-modules () {
      if test -L node_modules; then
        rm node_modules;
      elif test -e node_modules; then
        echo 'refusing to overwrite existing (non-symlinked) `node_modules`'
        exit 1
      fi

      ln -s ${nodeModules}/lib/node_modules node_modules
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
    export CARDANO_NODE_SOCKET_PATH="$PWD"/.node/socket/node.socket
    export CARDANO_NODE_CONFIG="$PWD"/.node-cfg/testnet/config/config.json

  '';
}
