{
  description = "cardano-transaction-lib";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # for the purescript project
    ogmios.url = "github:mlabs-haskell/ogmios/e406801eaeb32b28cd84357596ca1512bff27741";
    ogmios-datum-cache.url = "github:mlabs-haskell/ogmios-datum-cache/1c7a4af3f18bd3fa94a59e5a52e0ad6d974233e8";

    # Repository with network parameters
    cardano-configurations = {
      # Override with "path:/path/to/cardano-configurations";
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
  };

  outputs =
    { self
    , nixpkgs
    , haskell-nix
    , iohk-nix
    , cardano-configurations
    , ...
    }@inputs:
    let
      defaultSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      perSystem = nixpkgs.lib.genAttrs defaultSystems;
      overlay = with inputs; (final: prev:
        let
          inherit (prev) system;
        in
        {
          easy-ps =
            import inputs.easy-purescript-nix { pkgs = final; };
          ogmios-datum-cache =
            inputs.ogmios-datum-cache.defaultPackage.${system};
          ogmios = ogmios.packages.${system}."ogmios:exe:ogmios";
          ogmios-fixtures = ogmios;
          purescriptProject = import ./nix { inherit system; pkgs = final; };
          buildCtlRuntime = buildCtlRuntime final;
          launchCtlRuntime = launchCtlRuntime final;
          ctl-server = self.packages.${system}."ctl-server:exe:ctl-server";
          inherit cardano-configurations;
        });

      mkNixpkgsFor = system: import nixpkgs {
        overlays = [
          haskell-nix.overlay
          iohk-nix.overlays.crypto
          overlay
        ];
        inherit (haskell-nix) config;
        inherit system;
      };

      allNixpkgs = perSystem mkNixpkgsFor;
      nixpkgsFor = system: allNixpkgs.${system};

      defaultConfig = final: with final; {
        inherit (inputs) cardano-configurations;
        network = {
          name = "testnet";
          magic = 1097911063; # use `null` for mainnet
        };
        node = { port = 3001; };
        ogmios = { port = 1337; };
        ctlServer = { port = 8081; };
        postgres = {
          # User-facing port on host machine.
          # Can be set to null in order to not bind postgres port to host.
          # Postgres will always be accessible via `postgres:5432` from
          # containers.
          port = 5432;
          user = "ctxlib";
          password = "ctxlib";
          db = "ctxlib";
        };
        datumCache = {
          port = 9999;
          controlApiToken = "";
          blockFetcher = {
            firstBlock = {
              slot = 61625527;
              id = "3afd8895c7b270f8250b744ec8d2b3c53ee2859c9d5711d906c47fe51b800988";
            };
            autoStart = true;
            startFromLast = false;
            filter = builtins.toJSON { const = true; };
          };
        };
      };

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

      buildCtlRuntime = pkgs: extraConfig: { ... }:
        let
          inherit (builtins) toString;
          config = with pkgs.lib;
            fix (final: recursiveUpdate
              (defaultConfig final)
              (if isFunction extraConfig then extraConfig final else extraConfig));
          nodeDbVol = "node-${config.network.name}-db";
          nodeIpcVol = "node-${config.network.name}-ipc";
          nodeSocketPath = "/ipc/node.socket";
          server = pkgs.ctl-server;
          bindPort = port: "${toString port}:${toString port}";
        in
        with config;
        {
          docker-compose.raw = {
            volumes = {
              "${nodeDbVol}" = { };
              "${nodeIpcVol}" = { };
            };
          };
          services = {
            cardano-node = {
              service = {
                image = "inputoutput/cardano-node:1.35.0";
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
            ctl-server = {
              service = {
                useHostStore = true;
                ports = [ (bindPort ctlServer.port) ];
                depends_on = [ "ogmios" ];
                volumes = [ "${nodeIpcVol}:/ipc" ];
                command = [
                  "${pkgs.bash}/bin/sh"
                  "-c"
                  ''
                    ${server}/bin/ctl-server --port ${toString ctlServer.port}
                  ''
                ];
              };
            };
            postgres = {
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
            ogmios-datum-cache =
              let
                filter = nixpkgs.lib.strings.replaceStrings
                  [ "\"" "\\" ] [ "\\\"" "\\\\" ]
                  datumCache.blockFetcher.filter;
              in
              {
                service = {
                  useHostStore = true;
                  ports = [ (bindPort datumCache.port) ];
                  restart = "on-failure";
                  depends_on = [ "postgres" "ogmios" ];
                  command = [
                    "${pkgs.bash}/bin/sh"
                    "-c"
                    ''
                      ${pkgs.ogmios-datum-cache}/bin/ogmios-datum-cache \
                        --log-level warn \
                        --use-latest \
                        --server-api "${toString datumCache.controlApiToken}" \
                        --server-port ${toString datumCache.port} \
                        --ogmios-address ogmios \
                        --ogmios-port ${toString ogmios.port} \
                        --db-port 5432 \
                        --db-host postgres \
                        --db-user "${postgres.user}" \
                        --db-name "${postgres.db}" \
                        --db-password "${postgres.password}" \
                        --block-slot ${toString datumCache.blockFetcher.firstBlock.slot} \
                        --block-hash "${datumCache.blockFetcher.firstBlock.id}" \
                        --block-filter "${filter}"
                    ''
                  ];
                };
              };
          };
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
                (baseNameOf path) [ "server" "doc" ]
              );
          };
          ogmiosFixtures = buildOgmiosFixtures pkgs;
          project = pkgs.purescriptProject {
            inherit src pkgs projectName;
            packageJson = ./package.json;
            packageLock = ./package-lock.json;
            shell = {
              shellHook = exportOgmiosFixtures;
              packages = with pkgs; [
                ogmios
                ogmios-datum-cache
                nixpkgs-fmt
                fd
                arion
                haskellPackages.fourmolu
                nodePackages.prettier
                nodePackages.eslint
              ];
            };
          };
          exportOgmiosFixtures =
            ''
              export OGMIOS_FIXTURES="${ogmiosFixtures}"
            '';
        in
        rec {
          defaultPackage = packages.ctl-example-bundle-web;

          packages = {
            ctl-example-bundle-web = project.bundlePursProject {
              main = "Examples.Pkh2Pkh";
              entrypoint = "examples/index.js";
              htmlTemplate = "examples/index.html";
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
            ctl-unit-test = project.runPursTest {
              name = "ctl-unit-test";
              testMain = "Test.Unit";
              env = { OGMIOS_FIXTURES = "${ogmiosFixtures}"; };
            };
          };

          devShell = project.devShell;

          apps = {
            docs =
              let
                binPath = "docs-server";
                builtDocs = packages.docs;
                script = pkgs.writeShellApplication {
                  name = binPath;
                  runtimeInputs = [
                    pkgs.nodejs-14_x
                    pkgs.nodePackages.http-server
                  ];
                  text =
                    ''
                      ${pkgs.nodePackages.http-server}/bin/http-server \
                        ${builtDocs}/generated-docs/html
                    '';
                };
              in
              {
                type = "app";
                program = "${script}/bin/${binPath}";
              };
          };
        };

      hsProjectFor = pkgs: import ./server/nix {
        inherit inputs pkgs;
        inherit (pkgs) system;
        src = ./server;
      };
    in
    {
      inherit overlay;

      # flake from haskell.nix project
      hsFlake = perSystem (system: (hsProjectFor (nixpkgsFor system)).flake { });

      devShell = perSystem (system: self.devShells.${system}.ctl);

      devShells = perSystem (system: {
        # This is the default `devShell` and can be run without specifying
        # it (i.e. `nix develop`)
        ctl = (psProjectFor (nixpkgsFor system)).devShell;
        # It might be a good idea to keep this as a separate shell; if you're
        # working on the PS frontend, it doesn't make a lot of sense to pull
        # in all of the Haskell dependencies
        #
        # This can be used with `nix develop .#hsDevShell
        hsDevShell = self.hsFlake.${system}.devShell;
      });

      packages = perSystem (system:
        self.hsFlake.${system}.packages
        // (psProjectFor (nixpkgsFor system)).packages
      );

      apps = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        (psProjectFor pkgs).apps // {
          inherit (self.hsFlake.${system}.apps) "ctl-server:exe:ctl-server";
          ctl-runtime = pkgs.launchCtlRuntime { };
        });

      checks = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        (psProjectFor pkgs).checks
        // self.hsFlake.${system}.checks
        // {
          formatting-check = pkgs.runCommand "formatting-check"
            {
              nativeBuildInputs = with pkgs; [
                easy-ps.purs-tidy
                haskellPackages.fourmolu
                nixpkgs-fmt
                nodePackages.prettier
                fd
              ];
            }
            ''
              cd ${self}
              purs-tidy check $(fd -epurs)
              fourmolu -m check -o -XTypeApplications -o -XImportQualifiedPost \
                $(fd -ehs)
              nixpkgs-fmt --check $(fd -enix --exclude='spago*')
              prettier -c $(fd -ejs)
              touch $out
            '';

          js-lint-check = pkgs.runCommand "js-lint-check"
            {
              nativeBuildInputs = [
                pkgs.nodePackages.eslint
                pkgs.fd
              ];
            }
            ''
              cd ${self}
              eslint $(fd -ejs)
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

      defaultPackage = perSystem (system:
        (psProjectFor (nixpkgsFor system)).defaultPackage
      );

      hydraJobs = perSystem (system:
        self.checks.${system}
        // self.packages.${system}
        // self.devShells.${system}
      );
    };
}
