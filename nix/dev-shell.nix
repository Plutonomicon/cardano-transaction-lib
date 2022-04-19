{ pkgs
, system
, nodeModules
, nodejs
, compiler ? pkgs.easy-ps.purs-0_14_5
, ...
}:

pkgs.mkShell {
  buildInputs = [
    compiler
    pkgs.ogmios
    pkgs.cardano-cli
    pkgs.ogmios-datum-cache
    pkgs.easy-ps.spago
    pkgs.easy-ps.purs-tidy
    pkgs.easy-ps.purescript-language-server
    pkgs.easy-ps.pscid
    pkgs.easy-ps.spago2nix
    pkgs.nodePackages.node2nix
    nodejs
    pkgs.nixpkgs-fmt
    pkgs.fd
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

      ln -s ${pkgs.cardano-configurations}/network/testnet/cardano-node/config.json \
        "$cfgdir"/testnet/config/config.json
      ln -s ${pkgs.cardano-configurations}/network/testnet/genesis/byron.json \
        "$cfgdir"/testnet/genesis/byron.json
      ln -s ${pkgs.cardano-configurations}/network/testnet/genesis/shelley.json \
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
