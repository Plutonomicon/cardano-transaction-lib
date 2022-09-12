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
      rev = "0742820c84b964a894deda884677fd2b461458cf";
    };
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
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          ctl.overlays.purescript
          ctl.overlays.ctl-server
          ctl.overlays.runtime
        ];
      };
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
              nodePackages.eslint
              nodePackages.prettier
            ];
          };
        };
    in
    {
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
          ctl-scaffold-runtime = pkgs.buildCtlRuntime { };
        });

      apps = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        {
          default = self.apps.${system}.ctl-scaffold-runtime;
          ctl-scaffold-runtime = pkgs.launchCtlRuntime { };
          docs = (psProjectFor pkgs).launchSearchablePursDocs { };
        });

      checks = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        {
          ctl-scaffold-plutip-test = (psProjectFor pkgs).runPlutipTest {
            testMain = "Scaffold.Test.Main";
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
