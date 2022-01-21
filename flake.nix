{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.11";

    ogmios.url = "github:mlabs-haskell/ogmios";

    cardano-node = {
      type = "github";
      owner = "input-output-hk";
      repo = "cardano-node";
      rev = "ea8b632820db5546b22430bbb5ed8db4a2fef7dd";
    };

    cardano-configurations = {
      url = "github:input-output-hk/cardano-configurations";
      flake = false;
    };

    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , ogmios
    , cardano-node
    , cardano-configurations
    , easy-purescript-nix
    , ...
    }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];
      perSystem = nixpkgs.lib.genAttrs defaultSystems;
      nixpkgsFor = system: import nixpkgs {
        inherit system;
      };
      psLibFor = system:
        let
          pkgs = nixpkgsFor system;
          nodejs = nodejsFor system;
          nodeModules = nodeModulesFor system;
          easy-ps = easyPsFor system;
          spagoPkgs = spagoPkgsFor system;
        in
        import ./nix/lib.nix {
          inherit pkgs easy-ps spagoPkgs nodejs nodeModules;
        };
      # We should try to use a consistent version of node across all
      # project components
      nodejsFor = system: (nixpkgsFor system).nodejs-12_x;
      spagoPkgsFor = system: import ./spago-packages.nix {
        pkgs = nixpkgsFor system;
      };
      easyPsFor = system: import easy-purescript-nix {
        pkgs = nixpkgsFor system;
      };
      nodeModulesFor = system:
        let
          pkgs = nixpkgsFor system;
          nodejs = nodejsFor system;
          modules = pkgs.callPackage
            (_:
              let
                nodePkgs = import ./node2nix.nix {
                  inherit pkgs system nodejs;
                };
              in
              nodePkgs // {
                shell = nodePkgs.shell.override {
                  # see https://github.com/svanderburg/node2nix/issues/198
                  buildInputs = [ pkgs.nodePackages.node-gyp-build ];
                };
              });
        in
        (modules { }).shell.nodeDependencies;
    in
    {
      devShell = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          nodejs = nodejsFor system;
          nodeModules = nodeModulesFor system;
          easy-ps = easyPsFor system;
        in
        import ./nix/dev-shell.nix {
          inherit system pkgs inputs nodeModules easy-ps nodejs;
        }
      );

      packages = perSystem (system: {
        cardano-browser-tx = (psLibFor system).buildPursProject {
          name = "cardano-browser-tx";
          src = ./.;
        };
      });

      defaultPackage = perSystem (system:
        self.packages.${system}.cardano-browser-tx
      );

      # NOTE
      # Since we depend on two haskell.nix projects, `nix flake check`
      # is currently broken because of IFD issues
      checks = perSystem (system: {
        cardano-browser-tx = (psLibFor system).runPursTest {
          name = "cardano-browser-tx";
          src = ./.;
          subdir = ".";
        };
      });

      # TODO
      # Once we have a public ogmios instance to test against,
      # add `self.checks.${system}` to the `buildInputs`
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs = builtins.attrValues self.packages.${system};
          } "touch $out"
      );
    };
}
