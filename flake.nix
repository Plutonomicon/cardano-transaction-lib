{
  description = "cardano-transaction-lib";

  inputs = {
    # for the purescript project
    ogmios.url = "github:mlabs-haskell/ogmios/c4f896bf32ad066be8edd8681ee11e4ab059be7f";
    ogmios-datum-cache = {
      url = "github:mlabs-haskell/ogmios-datum-cache";
      flake = false;
    };
    # so named because we also need a different version of the repo below
    # in the server inputs and we use this one just for the `cardano-cli`
    # executables
    cardano-node-exe = {
      url = "github:input-output-hk/cardano-node/ea8b632820db5546b22430bbb5ed8db4a2fef7dd";
    };
    cardano-configurations = {
      url = "github:input-output-hk/cardano-configurations";
      flake = false;
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };

    # for the haskell server
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    haskell-nix.url = "github:mlabs-haskell/haskell.nix?ref=master";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    cardano-addresses = {
      url =
        "github:input-output-hk/cardano-addresses/d2f86caa085402a953920c6714a0de6a50b655ec";
      flake = false;
    };
    cardano-base = {
      url =
        "github:input-output-hk/cardano-base/41545ba3ac6b3095966316a99883d678b5ab8da8";
      flake = false;
    };
    cardano-config = {
      url =
        "github:input-output-hk/cardano-config/fe7855e981072d392513f9cf3994e0b6eba40d7d";
      flake = false;
    };
    cardano-crypto = {
      url =
        "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
      flake = false;
    };
    cardano-ledger = {
      url =
        "github:input-output-hk/cardano-ledger/1a9ec4ae9e0b09d54e49b2a40c4ead37edadcce5";
      flake = false;
    };
    cardano-node = {
      url =
        "github:input-output-hk/cardano-node/8909dea9b3996b8288f15f0e4f31fb0f63964197";
      flake = false;
    };
    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
      flake = false;
    };
    cardano-wallet = {
      url =
        "github:input-output-hk/cardano-wallet/ae7569293e94241ef6829139ec02bd91abd069df";
      flake = false;
    };
    ekg-forward = {
      url =
        "github:input-output-hk/ekg-forward/297cd9db5074339a2fb2e5ae7d0780debb670c63";
      flake = false;
    };
    flat = {
      url =
        "github:input-output-hk/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
    goblins = {
      url =
        "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };
    iohk-monitoring-framework = {
      url =
        "github:input-output-hk/iohk-monitoring-framework/808724ff8a19a33d0ed06f9ef59fbd900b08553c";
      flake = false;
    };
    optparse-applicative = {
      url =
        "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };
    ouroboros-network = {
      url =
        "github:input-output-hk/ouroboros-network/d2d219a86cda42787325bb8c20539a75c2667132";
      flake = false;
    };
    plutus = {
      url =
        "github:input-output-hk/plutus/1efbb276ef1a10ca6961d0fd32e6141e9798bd11";
      flake = false;
    };
    # NOTE
    # I don't we need anything from `plutus-apps`, so the following two are
    # not necessary. They might be useful for communicating with the frontend
    # however in case this is needed
    purescript-bridge = {
      url =
        "github:shmish111/purescript-bridge/6a92d7853ea514be8b70bab5e72077bf5a510596";
      flake = false;
    };
    servant-purescript = {
      url =
        "github:shmish111/servant-purescript/a76104490499aa72d40c2790d10e9383e0dbde63";
      flake = false;
    };
    Win32-network = {
      url =
        "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , haskell-nix
    , iohk-nix
    , ...
    }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];
      perSystem = nixpkgs.lib.genAttrs defaultSystems;
      overlay = system: with inputs; (prev: final: {
        easy-ps =
          import inputs.easy-purescript-nix { pkgs = prev; };
        ogmios-datum-cache =
          nixpkgs.legacyPackages.${system}.haskellPackages.callPackage
            ogmios-datum-cache
            { };
        ogmios = ogmios.packages.${system}."ogmios:exe:ogmios";
        cardano-cli = cardano-node-exe.packages.${system}.cardano-cli;
        purescriptProject = import ./nix { inherit system; pkgs = prev; };
        inherit cardano-configurations;
      });
      nixpkgsFor = system: import nixpkgs {
        overlays = [
          haskell-nix.overlay
          iohk-nix.overlays.crypto
          (overlay system)
        ];
        inherit (haskell-nix) config;
        inherit system;
      };
      psProjectFor = system:
        let
          pkgs = nixpkgsFor system;
          src = self;
          project = pkgs.purescriptProject {
            inherit src pkgs;
            shell = {
              packages = [
                pkgs.ogmios
                pkgs.cardano-cli
                pkgs.ogmios-datum-cache
                pkgs.nixpkgs-fmt
                pkgs.fd
              ];

              shellHook =
                let
                  nodeModules = project.mkNodeModules { };
                in
                ''
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

                  __ln-testnet-config

                  export CARDANO_NODE_SOCKET_PATH="$PWD"/.node/socket/node.socket
                  export CARDANO_NODE_CONFIG="$PWD"/.node-cfg/testnet/config/config.json

                '';
            };
          };
        in
        rec {
          defaultPackage = packages.cardano-transaction-lib;

          packages = {
            cardano-transaction-lib = project.buildPursProject {
              name = "cardano-transaction-lib";
              # Make sure the entire project compiles
              sources = [ "src" "test" "examples" ];
            };
          };

          # FIXME
          # Once we have ogmios/node instances available, we should also include a
          # test. This will need to be run via a Hercules `effect`
          checks = {
            ctl-unit-test = project.runPursTest {
              name = "ctl-unit-test";
              testMain = "Test.Unit";
              sources = [ "src" "test" "fixtures" ];
            };
          };

          devShell = project.devShell;
        };

      hsProjectFor = system:
        let
          pkgs = nixpkgsFor system;
          src = ./server;
        in
        import ./server/nix {
          inherit src inputs pkgs system;
        };
    in
    {
      # flake from haskell.nix project
      hsFlake = perSystem (system: (hsProjectFor system).flake { });

      devShell = perSystem (system: self.devShells.${system}.ctl);

      devShells = perSystem (system: {
        # This is the default `devShell` and can be run without specifying
        # it (i.e. `nix develop`)
        ctl = (psProjectFor system).devShell;
        # It might be a good idea to keep this as a separate shell; if you're
        # working on the PS frontend, it doesn't make a lot of sense to pull
        # in all of the Haskell dependencies
        #
        # This can be used with `nix develop .#hsDevShell
        hsDevShell = self.hsFlake.${system}.devShell;
      });

      packages = perSystem (system:
        self.hsFlake.${system}.packages
        // (psProjectFor system).packages
      );

      apps = perSystem (system: {
        inherit
          (self.hsFlake.${system}.apps) "ctl-server:exe:ctl-server";
      });

      checks = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        (psProjectFor system).checks
        // self.hsFlake.${system}.checks
        // {
          formatting-check = pkgs.runCommand "formatting-check"
            {
              nativeBuildInputs = with pkgs; [
                easy-ps.purs-tidy
                haskellPackages.fourmolu
                nixpkgs-fmt
                fd
              ];
            }
            ''
              cd ${self}
              purs-tidy check $(fd -epurs)
              fourmolu -m check -o -XTypeApplications -o -XImportQualifiedPost \
                $(fd -ehs)
              nixpkgs-fmt --check ./{flake,default,shell}.nix \
                 $(fd -enix --exclude='spago*')
              touch $out
            '';
        });

      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs =
              builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system};
          }
          ''
            touch $out
          ''
      );

      defaultPackage = perSystem (system: (psProjectFor system).defaultPackage);

      overlay = perSystem overlay;

      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
