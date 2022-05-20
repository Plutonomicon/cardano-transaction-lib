{ pkgs, system }:
{ src
, projectName
  # We should try to use a consistent version of node across all
  # project components
, nodejs ? pkgs.nodejs-14_x
, spagoPackages ? "${src}/spago-packages.nix"
, shell ? { }
, ...
}:
let
  purs = pkgs.easy-ps.purs-0_14_5;
  spagoPkgs = import spagoPackages { inherit pkgs; };
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

  shellFor =
    { packages ? [ ]
    , inputsFrom ? [ ]
    , shellHook ? ""
    , formatter ? "purs-tidy"
    , pursls ? true
    }:
      assert pkgs.lib.assertOneOf "formatter" formatter [ "purs-tidy" "purty" ];
      pkgs.mkShell {
        buildInputs = [
          purs
          nodejs
          pkgs.easy-ps.spago
          pkgs.easy-ps."${formatter}"
          pkgs.easy-ps.pscid
          pkgs.easy-ps.spago2nix
          pkgs.nodePackages.node2nix
        ] ++ pkgs.lib.lists.optional
          pursls
          pkgs.easy-ps.purescript-language-server;
        inherit packages inputsFrom;
        shellHook =
          let
            nodeModules = mkNodeModules { };
          in
          ''
            export NODE_PATH="${nodeModules}/lib/node_modules"
            export PATH="${nodeModules}/bin:$PATH"
          ''
          + shellHook;
      };

  buildPursDocsSearch =
    { name ? "purescript-docs-search"
    , ...
    }:
    pkgs.stdenv.mkDerivation {
      inherit name;
      srcs = [
        (pkgs.fetchurl {
          url = "https://github.com/purescript/purescript-docs-search/releases/download/v0.0.11/docs-search-app.js";
          sha256 = "17qngsdxfg96cka1cgrl3zdrpal8ll6vyhhnazqm4hwj16ywjm02";
        })
        (pkgs.fetchurl {
          url = "https://github.com/purescript/purescript-docs-search/releases/download/v0.0.11/purescript-docs-search";
          sha256 = "1hjdprm990vyxz86fgq14ajn0lkams7i00h8k2i2g1a0hjdwppq6";
        })
      ];
      buildInputs = [ nodejs ];
      unpackPhase = ''
        for srcFile in $srcs; do
          cp $srcFile $(stripHash $srcFile)
        done
      '';
      installPhase = ''
        chmod +x purescript-docs-search
        mkdir -p $out/bin
        mv docs-search-app.js purescript-docs-search $out/bin
      '';
    };

  buildPursProject =
    { sources ? [ "src" ]
    , withDevDeps ? false
    , name ? projectName
    , ...
    }:
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
        purs
        pkgs.easy-ps.spago
      ];
      unpackPhase =
        let
          srcs = builtins.concatStringsSep "," sources;
          # for e.g. `cp -r {a,b,c}` vs `cp -r a`
          srcsStr =
            if builtins.length sources > 1
            then ("{" + srcs + "}") else srcs;
        in
        ''
          export HOME="$TMP"
          export NODE_PATH="${nodeModules}/lib/node_modules"
          export PATH="${nodeModules}/bin:$PATH"
          cp -r $src/${srcsStr} .
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

  runPursTest =
    { testMain ? "Test.Main"
    , name ? "${projectName}-check"
    , srcs ? [ "src" "test" ]
    , ...
    }@args:
    (buildPursProject args).overrideAttrs
      (oas: {
        inherit name;
        doCheck = true;
        buildInputs = oas.buildInputs ++ [ nodejs ];
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

  buildPursDocs =
    { name ? "${projectName}-docs"
    , format ? "html"
    , ...
    }@args:
    (buildPursProject args).overrideAttrs
      (oas: {
        inherit name;
        buildPhase = ''
          purs docs --format ${format} "./**/*.purs" ".spago/*/*/src/**/*.purs"
        '';
        installPhase = ''
          mkdir $out
          cp -r generated-docs $out
          cp -r output $out
        '';
      });

  buildSearchablePursDocs =
    pkgs.stdenv.mkDerivation {
      name = "${projectName}-searchable-docs";
      dontUnpack = true;
      buildInputs = [
        spagoPkgs.installSpagoStyle
      ];
      buildPhase = ''
        cp -r ${buildPursDocs { format = "html"; }}/{generated-docs,output} .
        install-spago-style
        chmod -R +rwx .
        ${buildPursDocsSearch { }}/bin/purescript-docs-search build-index --package-name cardano-transaction-lib
      '';
      installPhase = ''
        mkdir $out
        cp -r generated-docs $out
      '';
    };

  bundlePursProject =
    { name ? "${projectName}-bundle-" +
        (if browserRuntime then "web" else "nodejs")
    , entrypoint ? "index.js"
    , htmlTemplate ? "index.html"
    , main ? "Main"
    , browserRuntime ? true
    , webpackConfig ? "webpack.config.js"
    , bundledModuleName ? "output.js"
    , ...
    }@args:
    (buildPursProject (args // { withDevDeps = true; })).overrideAttrs
      (oas: {
        inherit name;
        buildInputs = oas.buildInputs ++ [ nodejs ];
        buildPhase = ''
          ${pkgs.lib.optionalString browserRuntime "export BROWSER_RUNTIME=1"}
          build-spago-style "./**/*.purs"
          chmod -R +rwx .
          spago bundle-module --no-install --no-build -m "${main}" \
            --to ${bundledModuleName}
          cp $src/${entrypoint} .
          cp $src/${htmlTemplate} .
          cp $src/${webpackConfig} .
          mkdir ./dist
          webpack --mode=production -c ${webpackConfig} -o ./dist --entry ./${entrypoint}
        '';
        installPhase = ''
          mkdir $out
          mv dist $out
        '';
      });

in
{
  inherit buildPursProject runPursTest buildPursDocs buildSearchablePursDocs buildPursDocsSearch bundlePursProject;
  inherit purs nodejs mkNodeModules;
  devShell = shellFor shell;
}
