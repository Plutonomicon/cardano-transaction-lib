{ pkgs, system }:
{ src
  # We should try to use a consistent version of node across all
  # project components
, nodejs ? pkgs.nodejs-14_x
, spagoPkgsSrc
, ...
}:
let
  nodejs = pkgs.nodejs-14_x;
  pursCompiler = pkgs.easy-ps.purs-0_14_5;
  spagoPkgs = import spagoPkgsSrc {
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
        pursCompiler
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
{
  inherit mkNodeModules buildPursProject runPursTest pursCompiler nodejs;
}
