{ src, pkgs, system }:

let
  # We should try to use a consistent version of node across all
  # project components
  nodejs = pkgs.nodejs-14_x;
  compiler = pkgs.easy-ps.purs-0_14_5;
  spagoPkgs = import ../spago-packages.nix {
    inherit pkgs;
  };
  mkNodeEnv = { withDevDeps ? true }: import
    (pkgs.runCommand "nodePackages"
      {
        buildInputs = [ pkgs.nodePackages.node2nix ];
      } ''
      mkdir $out
      cp ${src}/package.json $out/package.json
      cp ${src}/package-lock.json $out/package-lock.json
      cd $out
      node2nix ${pkgs.lib.optionalString withDevDeps "--development" } \
        --lock package-lock.json
    '')
    { inherit pkgs nodejs system; };
  mkNodeModules = { withDevDeps ? true }:
    let
      nodeEnv = mkNodeEnv { inherit withDevDeps; };
      modules = pkgs.callPackage
        (_:
          nodeEnv // {
            shell = nodeEnv.shell.override {
              # see https://github.com/svanderburg/node2nix/issues/198
              buildInputs = [ pkgs.nodePackages.node-gyp-build ];
            };
          });
    in
    (modules { }).shell.nodeDependencies;

  buildPursProject = { name, src, withDevDeps ? false, ... }:
    let
      nodeModules = mkNodeModules { inherit withDevDeps; };
    in
    pkgs.stdenv.mkDerivation {
      inherit name src;
      buildInputs = [
        spagoPkgs.installSpagoStyle
        spagoPkgs.buildSpagoStyle
      ];
      nativeBuildInputs = [
        compiler
        pkgs.easy-ps.spago
      ];
      unpackPhase = ''
        export HOME="$TMP"
        cp -r ${nodeModules}/lib/node_modules .
        chmod -R u+rw node_modules
        cp -r $src/* .
        install-spago-style
      '';
      buildPhase = ''
        build-spago-style "./**/*.purs"
      '';
      installPhase = ''
        mkdir $out
        mv output $out/
      '';
    };

  runPursTest = { name, testMain ? "Test.Main", ... }@args:
    (buildPursProject args).overrideAttrs
      (oldAttrs: {
        name = "${name}-check";
        doCheck = true;
        buildInputs = oldAttrs.buildInputs ++ [ nodejs ];
        # spago will attempt to download things, which will fail in the
        # sandbox, so we can just use node instead
        # (idea taken from `plutus-playground-client`)
        checkPhase = ''
          node -e 'require("./output/${testMain}").main()'
        '';
        installPhase = ''
          touch $out
        '';
      });
in
rec {
  defaultPackage = packages.cardano-transaction-lib;

  packages = {
    cardano-transaction-lib = buildPursProject {
      name = "cardano-transaction-lib";
      inherit src;
    };
  };

  # NOTE
  # Since we depend on two haskell.nix projects, `nix flake check`
  # is currently broken because of IFD issues
  #
  # FIXME
  # Once we have ogmios/node instances available, we should also include a
  # test. This will need to be run via a Hercules `effect`
  checks = {
    ctl-unit-test = runPursTest {
      name = "ctl-unit-test";
      testMain = "Test.Unit";
      inherit src;
    };
  };

  # TODO
  # Once we have a public ogmios instance to test against,
  # add `self.checks.${system}` to the `buildInputs`
  check = pkgs.runCommand "combined-check"
    {
      nativeBuildInputs = builtins.attrValues packages;

    } "touch $out";

  devShell = pkgs.mkShell {
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

    shellHook =
      let
        nodeModules = mkNodeModules { };
      in
      ''
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
  };

}
