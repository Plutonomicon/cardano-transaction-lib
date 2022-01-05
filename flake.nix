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

    flake-utils.url = "github:numtide/flake-utils";

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
    , flake-utils
    , ...
    }@inputs:
    flake-utils.lib.eachSystem
      [ "x86_64-linux" "x86_64-darwin" ]
      (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          # We should try to use a consistent version of node across all
          # project components
          nodejs = pkgs.nodejs-12_x;

          ps-lib = import ./nix/lib.nix {
            inherit pkgs easy-ps spagoPkgs nodejs nodeModules;
          };

          easy-ps = import easy-purescript-nix { inherit pkgs; };

          spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

          nodeModules =
            let
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
          defaultPackage = self.packages.${system}.cardano-browser-tx;

          packages = {
            cardano-browser-tx = ps-lib.buildPursProject {
              name = "cardano-browser-tx";
              src = ./.;
            };
          };

          checks = {
            cardano-browser-tx = ps-lib.runPursTest {
              name = "cardano-browser-tx";
              src = ./.;
              subdir = ".";
            };
          };

          devShell = import ./nix/dev-shell.nix {
            inherit pkgs system inputs nodeModules easy-ps nodejs;
          };
        }
      );
}
