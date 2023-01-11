{
  description = "cardano-transaction-lib";

  nixConfig.bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]CTL@\\[\\033[33m\\]$(git rev-parse --abbrev-ref HEAD) \\[\\e[0;32m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";

  inputs = {
    nixpkgs.follows = "ogmios/nixpkgs";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    ogmios.url = "github:mlabs-haskell/ogmios/a7687bc03b446bc74564abe1873fbabfa1aac196";
    plutip.url = "github:mlabs-haskell/plutip?rev=8d1795d9ac3f9c6f31381104b25c71576eeba009";
    kupo-nixos.url = "github:mlabs-haskell/kupo-nixos/6f89cbcc359893a2aea14dd380f9a45e04c6aa67";
    kupo-nixos.inputs.kupo.follows = "kupo";

    kupo = {
      url = "github:CardanoSolutions/kupo/v2.2.0";
      flake = false;
    };

    ogmios-datum-cache.url = "github:mlabs-haskell/ogmios-datum-cache/862c6bfcb6110b8fe816e26b3bba105dfb492b24";

    # ogmios and ogmios-datum-cache nixos modules (remove and replace with the above after merging and updating)
    ogmios-nixos.url = "github:mlabs-haskell/ogmios";
    ogmios-datum-cache-nixos.url = "github:mlabs-haskell/ogmios-datum-cache/marton/nixos-module";

    cardano-node.follows = "ogmios-nixos/cardano-node";
    # for new environments like preview and preprod. TODO: remove this when cardano-node is updated
    iohk-nix-environments.url = "github:input-output-hk/iohk-nix";
    cardano-node.inputs.iohkNix.follows = "iohk-nix-environments";

    # Repository with network parameters
    cardano-configurations = {
      # Override with "path:/path/to/cardano-configurations";
      url = "github:input-output-hk/cardano-configurations";
      flake = false;
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix/da7acb2662961fd355f0a01a25bd32bf33577fa8";
      flake = false;
    };
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
            local parent="$(basename "$(dirname "$path")")"
            if command=$(pcregrep -o1 -o2 -o3 'Query\[(.*)\]|(EvaluateTx)|(SubmitTx)' <<< "$path")
            then
              echo "$path"
              json=$(jq -c .result "$path")
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
            ctl-example-bundle-web = project.bundlePursProject {
              main = "Ctl.Examples.ByUrl";
              entrypoint = "examples/index.js";
            };

            ctl-runtime = pkgs.arion.build {
              inherit pkgs;
              modules = [ (buildCtlRuntime pkgs { }) ];
            };

            docs = project.buildSearchablePursDocs {
              packageName = projectName;
            };
          };

          checks = {
            ctl-e2e-test = project.runE2ETest {
              name = "ctl-e2e-test";
              testMain = "Test.Ctl.E2E";
              env = { OGMIOS_FIXTURES = "${ogmiosFixtures}"; };
              buildInputs = [ inputs.kupo-nixos.packages.${pkgs.system}.kupo ];
            };
            ctl-plutip-test = project.runPlutipTest {
              name = "ctl-plutip-test";
              testMain = "Test.Ctl.Plutip";
              env = { OGMIOS_FIXTURES = "${ogmiosFixtures}"; };
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
            docs = project.launchSearchablePursDocs {
              builtDocs = packages.docs;
            };
          };
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
              version = "0.20.7";
              src =
                if final.stdenv.isDarwin
                then
                  final.fetchurl
                    {
                      url = "https://github.com/purescript/spago/releases/download/${version}/macOS.tar.gz";
                      sha256 = "0s5zgz4kqglsavyh7h70zmn16vayg30alp42w3nx0zwaqkp79xla";
                    }
                else
                  final.fetchurl {
                    url = "https://github.com/purescript/spago/releases/download/${version}/Linux.tar.gz";
                    sha256 = "0bh15dr1fg306kifqipnakv3rxab7hjfpcfzabw7vmg0gsfx8xka";
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
                  inputs.plutip.packages.${system}."plutip:exe:plutip-server";
                ogmios-datum-cache =
                  inputs.ogmios-datum-cache.defaultPackage.${system};
                ogmios = ogmios.packages.${system}."ogmios:exe:ogmios";
                kupo = inputs.kupo-nixos.packages.${system}.kupo;
                buildCtlRuntime = buildCtlRuntime final;
                launchCtlRuntime = launchCtlRuntime final;
                inherit cardano-configurations;
              }
          );
      };

      devShells = perSystem (system: {
        # This is the default `devShell` and can be run without specifying
        # it (i.e. `nix develop`)
        default = (psProjectFor (nixpkgsFor system)).devShell;
      });

      packages = perSystem (system:
        (psProjectFor (nixpkgsFor system)).packages
      );

      apps = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        (psProjectFor pkgs).apps // {
          ctl-runtime = pkgs.launchCtlRuntime { };
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
          {
            services.ogmios.package =
              inputs.ogmios.packages.x86_64-linux."ogmios:exe:ogmios";
          }
          inputs.kupo-nixos.nixosModules.kupo
          inputs.ogmios-datum-cache-nixos.nixosModules.ogmios-datum-cache
          {
            services.ogmios-datum-cache.package =
              inputs.ogmios-datum-cache.packages.x86_64-linux."ogmios-datum-cache";
          }
          ./nix/test-nixos-configuration.nix
        ];
        specialArgs = {
          inherit (inputs) cardano-configurations;
        };
      };

      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
