{ pkgs
, easy-ps
, spagoPkgs
, nodejs
, compiler ? easy-ps.purs-0_14_5
, nodeModules
, ...
}:
rec {
  buildPursProject = { name, src, subdir ? "src" }:
    pkgs.stdenv.mkDerivation {
      inherit name src;
      buildInputs = [
        spagoPkgs.installSpagoStyle
        spagoPkgs.buildSpagoStyle
      ];
      nativeBuildInputs = [
        compiler
        easy-ps.spago
      ];
      buildPhase = ''
        export HOME="$TMP"

        cp -r ${nodeModules}/lib/node_modules .
        chmod -R u+rw node_modules
        cp $src/spago.dhall .
        cp $src/packages.dhall .
        cp -r $src/${subdir} .

        install-spago-style
        build-spago-style "./${subdir}/**/*.purs"
      '';
      installPhase = ''
        mkdir $out
        mv output $out/
      '';
    };

  runPursTest = { name, ... }@args:
    (buildPursProject args).overrideAttrs
      (oldAttrs: {
        name = "${name}-check";
        doCheck = true;
        buildInputs = oldAttrs.buildInputs ++ [ nodejs ];
        # spago will attempt to download things, which will fail in the
        # sandbox (idea taken from `plutus-playground-client`)
        checkPhase = ''
          node -e 'require("./output/Test.Main").main()'
        '';
        installPhase = ''
          touch $out
        '';
      });

  bundlePursProject = { name, ... }@args:
    (buildPursProject args).overrideAttrs
      (oldAttrs: {
        name = "${name}-bundled";
        installPhase = ''
          spago bundle-module --no-install --no-build --to $out/index.js
        '';
      });
}
