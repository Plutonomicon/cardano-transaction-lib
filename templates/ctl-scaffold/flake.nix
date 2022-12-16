{
  description = "ctl-scaffold";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    ctl = {
      type = "github";
      owner = "Plutonomicon";
      repo = "cardano-transaction-lib";
      rev = "27f997461fda4a6f7eb52f1165a91d7d453fb990";
    };
    # To use the same version of `nixpkgs` as we do
    nixpkgs.follows = "ctl/nixpkgs";
  };

  outputs = { self, nixpkgs, ctl, ... }@inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      # generate `pkgs` with CTL's overlays applied. This gives you access to
      # various additional packages. The versions are the same as those that CTL uses.
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          ctl.overlays.purescript
          ctl.overlays.runtime
          ctl.overlays.spago
        ];
      };


      # The configuration for the CTL runtime, which will be passed to the
      # expression that builds the JSON file used by Arion. This value can be
      # shared between `buildCtlRuntime` and `launchCtlRuntime`, as shown below
      #
      # You can refer to the final configuration value by passing a function
      # that takes a single arugment. Alternatively, you can pass an attrset
      # directly.
      #
      # Here we demonstrate how to add `extraServices` and `extraDockerCompose`.
      # For the other attributes, which have default values,
      # consult `defaultConfig` in `nix/runtime.nix`.
      runtimeConfig = { };
      # runtimeConfig = final: with final; {
      #   # You can add new services to the runtime. These should correspond to
      #   # Arion's `service` definition. The default is the empty attribute set
      #   extraServices = {
      #     # an image from dockerhub
      #     foo = {
      #       service = {
      #         image = "bar:foo";
      #         command = [
      #           "baz"
      #           "--quux"
      #         ];
      #       };

      #       # Or a Nix-based image
      #       foo2 = {
      #         service = {
      #           useHostStore = true;
      #           command = [
      #             "${(nixpkgsFor system).baz}/bin/baz"
      #             "--quux"
      #           ];
      #         };
      #       };
      #     };
      #   };
      #   # This corresponds to `docker-compose.raw` from Arion. You can add new
      #   # volumes, etc... using this
      #   extraDockerCompose = { volumes = { someVol = { }; }; };

      psProjectFor = pkgs:
        pkgs.purescriptProject rec {
          inherit pkgs;
          projectName = "ctl-scaffold";
          packageJson = ./package.json;
          packageLock = ./package-lock.json;
          src = builtins.path {
            path = ./.;
            name = "${projectName}-src";
            # Adjust the `filter` as necessary
            filter = path: ftype: !(pkgs.lib.hasSuffix ".md" path);
          };
          shell = {
            withRuntime = true;
            packageLockOnly = true;
            packages = with pkgs; [
              fd
              nodePackages.eslint
              nodePackages.prettier
            ];
          };
        };
    in
    {
      # `buildCtlRuntime` will generate a Nix expression that, when built with
      # `pkgs.arion.build`, outputs a JSON file compatible with Arion. This can
      # be run directly with Arion or passed to another derivation. Or you can
      # use `buildCtlRuntime` with `runArion` (from the `hercules-ci-effects`)
      # library
      #
      # Use `nix build .#<PACKAGE>` to build. To run with Arion (i.e. in your
      # shell): `arion --prebuilt-file ./result up`
      packages = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        {
          default = self.packages.${system}.ctl-scaffold-bundle-web;
          ctl-scaffold-bundle-web = (psProjectFor pkgs).bundlePursProject {
            main = "Scaffold.Main";
            entrypoint = "index.js";
          };
          ctl-scaffold-runtime = pkgs.buildCtlRuntime runtimeConfig;
        });

      # `launchCtlRuntime` will generate a Nix expression from the provided
      # config, build it into a JSON file, and then run it with Arion
      #
      # Use `nix run .#<APP>` to run the services (e.g. `nix run .#ctl-runtime`)
      apps = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        {
          default = self.apps.${system}.ctl-scaffold-runtime;
          ctl-scaffold-runtime = pkgs.launchCtlRuntime runtimeConfig;
          docs = (psProjectFor pkgs).launchSearchablePursDocs { };
        });

      checks = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        {
          ctl-scaffold-plutip-test = (psProjectFor pkgs).runPlutipTest {
            testMain = "Test.Scaffold.Main";
          };

          formatting-check = pkgs.runCommand "formatting-check"
            {
              nativeBuildInputs = with pkgs; [
                fd
                easy-ps.purs-tidy
                nixpkgs-fmt
                nodePackages.prettier
              ];
            }
            ''
              cd ${self}
              purs-tidy check $(fd -epurs)
              nixpkgs-fmt --check $(fd -enix --exclude='spago*')
              prettier -c $(fd -ejs)
              touch $out
            '';

          js-lint-check = pkgs.runCommand "js-lint-check"
            {
              nativeBuildInputs = [ pkgs.nodePackages.eslint pkgs.fd ];
            }
            ''
              cd ${self}
              eslint $(fd -ejs)
              touch $out
            '';
        });

      devShells = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        {
          default = (psProjectFor pkgs).devShell;
        });
    };
}
