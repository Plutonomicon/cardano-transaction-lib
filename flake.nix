{
  description = "cardano-transaction-lib";

  nixConfig = {
    extra-substituters = [ "https://plutonomicon.cachix.org" ];
    extra-trusted-public-keys = [ "plutonomicon.cachix.org-1:evUxtNULjCjOipxwAnYhNFeF/lyYU1FeNGaVAnm+QQw=" ];
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]CTL@\\[\\033[33m\\]$(git rev-parse --abbrev-ref HEAD) \\[\\e[0;32m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  };

  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    hackage-nix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackage-nix";
    };
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    cardano-node.url = "github:input-output-hk/cardano-node/8.1.1";

    ogmios-nixos = {
      url = "github:mlabs-haskell/ogmios-nixos/78e829e9ebd50c5891024dcd1004c2ac51facd80";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        iohk-nix.follows = "iohk-nix";
        haskell-nix.follows = "haskell-nix";
        cardano-node.follows = "cardano-node";
        ogmios-src.follows = "ogmios";
      };
    };

    ogmios = {
      url = "github:CardanoSolutions/ogmios/v6.0.0";
      flake = false;
    };

    kupo-nixos = {
      url = "github:mlabs-haskell/kupo-nixos/6f89cbcc359893a2aea14dd380f9a45e04c6aa67";
      inputs.kupo.follows = "kupo";
    };

    kupo = {
      url = "github:CardanoSolutions/kupo/v2.2.0";
      flake = false;
    };

    # Repository with network parameters
    # NOTE(bladyjoker): Cardano configurations (yaml/json) often change format and break, that's why we pin to a specific known version.
    cardano-configurations = {
      # Override with "path:/path/to/cardano-configurations";
      url = "github:input-output-hk/cardano-configurations?rev=d952529afdfdf6d53ce190b1bf8af990a7ae9590";
      flake = false;
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };

    blockfrost.url = "github:blockfrost/blockfrost-backend-ryo/v1.7.0";
    db-sync.url = "github:input-output-hk/cardano-db-sync/13.1.0.0";

    # Plutip server related inputs
    plutip = {
      url = "github:mlabs-haskell/plutip?ref=gergely/version-bump";
      # TODO(bladyjoker): Why are we overriding inputs here?
      inputs = {
        nixpkgs.follows = "nixpkgs";
        iohk-nix.follows = "iohk-nix";
        haskell-nix.follows = "haskell-nix";
        hackage-nix.follows = "hackage-nix";
        cardano-node.follows = "cardano-node";
      };
    };

    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
  };

  outputs =
    { self
    , nixpkgs
    , cardano-configurations
    , ...
    }@inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      mkNixpkgsFor = system: import nixpkgs {
        overlays = nixpkgs.lib.attrValues self.overlays ++ [
          (_: _: {
            ogmios-fixtures = inputs.ogmios;
          })
        ];
        inherit system;
      };

      inherit (import ./nix/runtime.nix { inherit inputs; })
        buildCtlRuntime launchCtlRuntime;

      allNixpkgs = perSystem mkNixpkgsFor;

      nixpkgsFor = system: allNixpkgs.${system};

      buildOgmiosFixtures = pkgs: pkgs.runCommand "ogmios-fixtures"
        {
          buildInputs = [ pkgs.jq pkgs.pcre ];
        }
        ''
          cp -r ${pkgs.ogmios-fixtures}/server/test/vectors vectors
          chmod -R +rwx .

          function on_file () {
            local path=$1
            match_A=$(pcregrep -o1 'QueryLedgerState([a-zA-Z]+)\/' <<< "$path")
            match_B=$(pcregrep -o1 '([a-zA-Z]+)Response' <<< "$path")
            command=""
            if [ ! -z $match_A ]
            then
              command="QueryLedgerState-$match_A"
            elif [ ! -z $match_B ]
            then
              command="$match_B"
            fi
            if [ ! -z $command ]
            then
              echo "$path"
              json=$(cat "$path")
              md5=($(md5sum <<< "$json"))
              printf "%s" "$json" > "ogmios/$command-$md5.json"
            fi
          }
          export -f on_file

          mkdir ogmios
          find vectors/ -type f -name "*.json" -exec bash -c 'on_file "{}"' \;
          mkdir $out
          cp -rT ogmios $out
        '';

      psProjectFor = pkgs:
        let
          projectName = "cardano-transaction-lib";
          # `filterSource` will still trigger rebuilds with flakes, even if a
          # filtered path is modified as the output path name is impurely
          # derived. Setting an explicit `name` with `path` helps mitigate this
          src = builtins.path {
            path = self;
            name = "${projectName}-src";
            filter = path: ftype:
              !(pkgs.lib.hasSuffix ".md" path)
              && !(ftype == "directory" && builtins.elem
                (baseNameOf path) [ "doc" ]
              );
          };
          ogmiosFixtures = buildOgmiosFixtures pkgs;
          project = pkgs.purescriptProject {
            inherit src pkgs projectName;
            packageJson = ./package.json;
            packageLock = ./package-lock.json;
            shell = {
              withRuntime = true;
              shellHook = exportOgmiosFixtures;
              packageLockOnly = true;
              packages = with pkgs; [
                arion
                fd
                nixpkgs-fmt
                nodePackages.eslint
                nodePackages.prettier
                blockfrost-backend-ryo
              ];
            };
          };
          exportOgmiosFixtures =
            ''
              export OGMIOS_FIXTURES="${ogmiosFixtures}"
            '';
        in
        rec {
          packages = {
            ctl-purs-project = project.buildPursProject { };

            ctl-example-bundle-web-esbuild = project.bundlePursProjectEsbuild {
              main = "Ctl.Examples.ByUrl";
            };

            ctl-example-bundle-web-webpack = project.bundlePursProjectWebpack {
              main = "Ctl.Examples.ByUrl";
            };

            ctl-runtime = pkgs.arion.build {
              inherit pkgs;
              modules = [ (buildCtlRuntime pkgs { }) ];
            };

            docs = project.buildPursDocs {
              packageName = projectName;
            };
          };

          checks = {
            ctl-e2e-test = project.runE2ETest {
              name = "ctl-e2e-test";
              runnerMain = "Test.Ctl.E2E";
              testMain = "Ctl.Examples.ByUrl";
              buildInputs = [ inputs.kupo-nixos.packages.${pkgs.system}.kupo ];
            };
            ctl-plutip-test = project.runPlutipTest {
              name = "ctl-plutip-test";
              testMain = "Test.Ctl.Plutip";
            };
            ctl-staking-test = project.runPlutipTest {
              name = "ctl-staking-test";
              testMain = "Test.Ctl.Plutip.Staking";
            };
            ctl-unit-test = project.runPursTest {
              name = "ctl-unit-test";
              testMain = "Test.Ctl.Unit";
              env = { OGMIOS_FIXTURES = "${ogmiosFixtures}"; };
            };
          };

          devShell = project.devShell;

          apps = {
            # TODO: restore this
            # https://github.com/Plutonomicon/cardano-transaction-lib/issues/1578
            # docs = project.launchSearchablePursDocs {
            #   builtDocs = packages.docs;
            # };
          };
        };

      plutipServerFor = system:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              inputs.haskell-nix.overlay
              inputs.iohk-nix.overlays.crypto
            ];
          };
        in
        import ./plutip-server {
          inherit pkgs;
          inherit (inputs) plutip CHaP cardano-node;
          inherit (pkgs) system;
          src = ./plutip-server;
        };
    in
    {
      overlay = builtins.trace
        (
          "warning: `cardano-transaction-lib.overlay` is deprecated and will be"
          + " removed in the next release. Please use"
          + " `cardano-transaction-lib.overlays.{runtime, purescript}`"
          + " directly instead"
        )
        nixpkgs.lib.composeManyExtensions
        (nixpkgs.lib.attrValues self.overlays);

      overlays = with inputs; {
        purescript = final: prev: {
          easy-ps = import inputs.easy-purescript-nix { pkgs = final; };
          purescriptProject = import ./nix { pkgs = final; };
        };
        spago = final: prev: {
          easy-ps = prev.easy-ps // {
            spago = prev.easy-ps.spago.overrideAttrs (_: rec {
              version = "0.21.0";
              src =
                if final.stdenv.isDarwin
                then
                  final.fetchurl
                    {
                      url = "https://github.com/purescript/spago/releases/download/${version}/macOS.tar.gz";
                      sha256 = "19c0kdg7gk1c7v00lnkcsxidffab84d50d6l6vgrjy4i86ilhzd5";
                    }
                else
                  final.fetchurl {
                    url = "https://github.com/purescript/spago/releases/download/${version}/Linux.tar.gz";
                    sha256 = "1klczy04vwn5b39cnxflcqzap0d5kysp4dsw73i95xm5m7s37049";
                  };
            });
          };
        };
        runtime =
          (
            final: prev:
              let
                inherit (prev) system;
              in
              {
                plutip-server =
                  (plutipServerFor system).hsPkgs.plutip-server.components.exes.plutip-server;
                ogmios = ogmios-nixos.packages.${system}."ogmios:exe:ogmios";
                kupo = inputs.kupo-nixos.packages.${system}.kupo;
                cardano-db-sync = inputs.db-sync.packages.${system}.cardano-db-sync;
                blockfrost-backend-ryo = inputs.blockfrost.packages.${system}.blockfrost-backend-ryo;
                buildCtlRuntime = buildCtlRuntime final;
                launchCtlRuntime = launchCtlRuntime final;
                inherit cardano-configurations;
              }
          );
      };

      # flake from haskell.nix project
      hsFlake = perSystem (system: (plutipServerFor system).flake { });

      devShells = perSystem (system: {
        # This is the default `devShell` and can be run without specifying
        # it (i.e. `nix develop`)
        default = (psProjectFor (nixpkgsFor system)).devShell;

        # This can be used with `nix develop .#devPlutipServer` to work with `./plutip-server`
        devPlutipServer = ((plutipServerFor system).flake { }).devShell;
      });

      packages = perSystem (system:
        (psProjectFor (nixpkgsFor system)).packages
        // ((plutipServerFor system).flake { }).packages
      );

      apps = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        (psProjectFor pkgs).apps // {
          ctl-runtime = pkgs.launchCtlRuntime { };
          ctl-runtime-blockfrost = pkgs.launchCtlRuntime { blockfrost.enable = true; };
          default = self.apps.${system}.ctl-runtime;
          vm = {
            type = "app";
            program =
              "${self.nixosConfigurations.test.config.system.build.vm}/bin/run-nixos-vm";
          };
        });

      # TODO
      # Add a check that attempts to verify if the scaffolding template is
      # reasonably up-to-date. See:
      # https://github.com/Plutonomicon/cardano-transaction-lib/issues/839
      checks = perSystem (system:
        let
          pkgs = nixpkgsFor system;

        in
        (psProjectFor pkgs).checks
        // {
          formatting-check = pkgs.runCommand "formatting-check"
            {
              nativeBuildInputs = with pkgs; [
                easy-ps.purs-tidy
                nixpkgs-fmt
                nodePackages.prettier
                nodePackages.eslint
                fd
              ];
            }
            ''
              cd ${self}
              make check-format
              touch $out
            '';
          template-deps-json = pkgs.runCommand "template-deps-check"
            {
              ctlPackageJson = builtins.readFile ./package.json;
              ctlScaffoldPackageJson = builtins.readFile ./templates/ctl-scaffold/package.json;
              nativeBuildInputs = [ pkgs.jq ];
            } ''
            cd ${self}
            diff <(jq -S .dependencies <<< $ctlPackageJson) <(jq -S .dependencies <<< $ctlScaffoldPackageJson)
            # We don't want to include `doctoc` in the template dev dependencies.
            diff \
              <(jq -S '.devDependencies | del(.doctoc)' <<< $ctlPackageJson) \
              <(jq -S .devDependencies <<< $ctlScaffoldPackageJson)
            touch $out
          '';
          template-dhall-diff = pkgs.runCommand "template-dhall-diff-check"
            (with builtins;
            let
              ctlPkgsExp = import ./spago-packages.nix { inherit pkgs; };
              ctlScaffoldPkgsExp = import ./templates/ctl-scaffold/spago-packages.nix { inherit pkgs; };
              ctlPs = attrValues ctlPkgsExp.inputs;
              ctlScaffoldPs = filter (p: p.name != "cardano-transaction-lib")
                (attrValues ctlScaffoldPkgsExp.inputs);
              intersection = pkgs.lib.lists.intersectLists ctlPs ctlScaffoldPs;
              scaffoldDisjoint = pkgs.lib.lists.subtractLists intersection ctlScaffoldPs;
              ctlDisjoint = pkgs.lib.lists.subtractLists intersection ctlPs;
            in
            {
              inherit ctlDisjoint scaffoldDisjoint;
              nativeBuildInputs = [ ];
            }
            ) ''

            if [ -z "$ctlDisjoint" ] && [ -z "$scaffoldDisjoint" ];
            then
              touch $out
            else
              if [ -n "$ctlDisjoint" ];
              then
                echo "The following packages are in the main projects dependencies but not in the scaffold:"
                for p in $ctlDisjoint; do
                  echo "  $p"
                done
              fi
              if [ -n "$scaffoldDisjoint" ];
              then
                echo "The following packages are in the scaffold projects dependencies but not in the main:"
                for p in $scaffoldDisjoint; do
                  echo "  $p"
                done
              fi
              exit 1
            fi
          '';
          template-version = pkgs.runCommand "template-consistent-version-check"
            (
              let
                ctlScaffoldPackages = import ./templates/ctl-scaffold/spago-packages.nix { inherit pkgs; };
                ctlScaffoldFlake = import ./templates/ctl-scaffold/flake.nix;
                versionCheck = ctlScaffoldPackages.inputs."cardano-transaction-lib".version == ctlScaffoldFlake.inputs.ctl.rev;
              in
              {
                packagesLibRev = ctlScaffoldPackages.inputs."cardano-transaction-lib".version;
                flakeLibRev = ctlScaffoldFlake.inputs.ctl.rev;
                nativeBuildInputs = [ ];
              }
            ) ''

            if [ $packagesLibRev != $flakeLibRev ]
            then
              echo "CTL revision in scaffold flake.nix ($flakeLibRev) doesn't match revision referenced in spago-packages.nix ($packagesLibRev). Please update flake.nix or packages.dhall and run spago2nix."
              exit 1
            fi
            touch $out
          '';
          examples-imports-check = pkgs.runCommand "examples-imports-check" { }
            ''
              cd ${self}
              make check-examples-imports
              touch $out
            '';
        });

      templatePath = builtins.toString self + self.templates.ctl-scaffold.path;

      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            combined =
              builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system};
          }
          ''
            echo $combined
            touch $out
          ''
      );

      templates = {
        default = self.templates.ctl-scaffold;
        ctl-scaffold = {
          path = ./templates/ctl-scaffold;
          description = "A minimal CTL-based scaffold project";
          welcomeText = ''
            Welcome to your new CTL project!

            To enter the Nix environment and start working on it, run `nix develop`

            Please also see our

            - [Documentation](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/doc)

            - [Generated docs](https://plutonomicon.github.io/cardano-transaction-lib/)

            - [Discord server](https://discord.gg/JhbexnV9Pc)

            If you encounter problems and/or want to report a bug, you can open
            an issue [here](https://github.com/Plutonomicon/cardano-transaction-lib/issues).

            Please search for existing issues beforehand!

          '';
        };
      };

      nixosConfigurations.test = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          inputs.cardano-node.nixosModules.cardano-node
          inputs.ogmios-nixos.nixosModules.ogmios
          inputs.kupo-nixos.nixosModules.kupo
          ./nix/test-nixos-configuration.nix
        ];
        specialArgs = {
          inherit (inputs) cardano-configurations;
        };
      };

      herculesCI = inputs.hercules-ci-effects.lib.mkHerculesCI { inherit inputs; } {
        hercules-ci.flake-update = {
          enable = true;
          updateBranch = "updated-flake-lock";
          createPullRequest = true;
          autoMergeMethod = null;
          when = {
            minute = 00;
            hour = 12;
            dayOfWeek = "Sun";
          };
        };

        herculesCI.ciSystems = [ "x86_64-linux" ];
      };
    };
}
