{ src
, pkgs
, system
, inputs
, self
}:

let
  ps-lib = import ./lib.nix {
    inherit pkgs spagoPkgs nodejs nodeModules;
  };
  # We should try to use a consistent version of node across all
  # project components
  nodejs = pkgs.nodejs-12_x;
  spagoPkgs = import ../spago-packages.nix { inherit pkgs; };
  nodeEnv = import
    (pkgs.runCommand "nodePackages"
      {
        buildInputs = [ pkgs.nodePackages.node2nix ];
      } ''
      mkdir $out
      cp ${src}/package.json $out/package.json
      cp ${src}/package-lock.json $out/package-lock.json
      cd $out
      node2nix --lock package-lock.json
    '')
    { inherit pkgs nodejs system; };
  nodeModules =
    let
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
in
{
  defaultPackage = self.packages.${system}.cardano-transaction-lib;

  packages = {
    cardano-transaction-lib = ps-lib.buildPursProject {
      name = "cardano-transaction-lib";
      inherit src;
    };
  };

  # NOTE
  # Since we depend on two haskell.nix projects, `nix flake check`
  # is currently broken because of IFD issues
  checks = {
    cardano-transaction-lib = ps-lib.runPursTest {
      name = "cardano-transaction-lib";
      subdir = builtins.toString src;
      inherit src;
    };
  };

  # TODO
  # Once we have a public ogmios instance to test against,
  # add `self.checks.${system}` to the `buildInputs`
  check = pkgs.runCommand "combined-check"
    {
      nativeBuildInputs = builtins.attrValues self.packages.${system};

    } "touch $out";

  devShell = import ./dev-shell.nix {
    inherit pkgs system inputs nodeModules nodejs;
  };
}
