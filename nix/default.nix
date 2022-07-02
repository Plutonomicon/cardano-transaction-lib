{ pkgs, system }:
{ src
, projectName
  # package.json
, packageJson
  # package-lock.json
, packageLock
, psSources ? [ "src" "test" ]
, censorCodes ? [ "UserDefinedWarning" ]
  # We should try to use a consistent version of node across all
  # project components
, nodejs ? pkgs.nodejs-14_x
, spagoPackages ? "${src}/spago-packages.nix"
, shell ? { }
, ...
}@args:
let
  purs = pkgs.easy-ps.purs-0_14_5;
  spagoPkgs = import spagoPackages { inherit pkgs; };
  mkNodeEnv = { withDevDeps ? true }: import
    (pkgs.runCommand "node-packages-${projectName}"
      {
        buildInputs = [ pkgs.nodePackages.node2nix ];
      } ''
      mkdir $out
      cd $out
      cp ${packageLock} ./package-lock.json
      cp ${packageJson} ./package.json
      node2nix ${pkgs.lib.optionalString withDevDeps "--development" } \
        --lock ./package-lock.json -i ./package.json
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

  projectNodeModules = mkNodeModules { };

  sepWithComma = builtins.concatStringsSep ",";

  # for e.g. `cp -r {a,b,c}` vs `cp -r a`
  joinSources = srcs:
    let
      withCommas = sepWithComma srcs;
    in
    if builtins.length srcs > 1
    then ("{" + withCommas + "}")
    else withCommas;

  shellFor =
    { packages ? [ ]
    , inputsFrom ? [ ]
    , shellHook ? ""
    , formatter ? "purs-tidy"
    , pursls ? true
    , nodeModules ? projectNodeModules
    }:
      assert pkgs.lib.assertOneOf "formatter" formatter [ "purs-tidy" "purty" ];
      pkgs.mkShell {
        buildInputs = [
          nodeModules
          purs
          nodejs
          pkgs.easy-ps.spago
          pkgs.easy-ps."${formatter}"
          pkgs.easy-ps.pscid
          pkgs.easy-ps.psa
          pkgs.easy-ps.spago2nix
          pkgs.nodePackages.node2nix
        ] ++ pkgs.lib.lists.optional
          pursls
          pkgs.easy-ps.purescript-language-server;
        inherit packages inputsFrom;
        shellHook = ''
          export NODE_PATH="${nodeModules}/lib/node_modules"
          export PATH="${nodeModules}/bin:$PATH"
        ''
        + shellHook;
      };

  buildPursProject =
    { name ? projectName
    , psaCensorCodes ? censorCodes
    , nodeModules ? projectNodeModules
    , ...
    }:
    let
      # This is what spago2nix does
      spagoGlob = pkg:
        ''".spago/${pkg.name}/${pkg.version}/src/**/*.purs"'';
      spagoGlobs = builtins.toString (
        builtins.map spagoGlob (builtins.attrValues spagoPkgs.inputs)
      );
    in
    pkgs.stdenv.mkDerivation {
      inherit name src;
      buildInputs = [
        nodeModules
        spagoPkgs.installSpagoStyle
        pkgs.easy-ps.psa
      ];
      nativeBuildInputs = [
        purs
        pkgs.easy-ps.spago
      ];
      unpackPhase = ''
        export HOME="$TMP"
        export NODE_PATH="${nodeModules}/lib/node_modules"
        export PATH="${nodeModules}/bin:$PATH"
        cp -r $src/${joinSources psSources} .
        install-spago-style
      '';
      buildPhase = ''
        psa --strict --censor-lib --is-lib=.spago ${spagoGlobs} \
          --censor-codes=${sepWithComma psaCensorCodes} "./**/*.purs"
      '';
      installPhase = ''
        mkdir $out
        mv output $out/
      '';
    };

  compiledProject = buildPursProject { };

  runPursTest =
    { testMain ? "Test.Main"
    , name ? "${projectName}-check"
    , project ? compiledProject
    , nodeModules ? projectNodeModules
    , ...
    }@args: pkgs.runCommand "${name}"
      {
        buildInputs = [ project nodeModules ];
      }
      # spago will attempt to download things, which will fail in the
      # sandbox, so we can just use node instead
      # (idea taken from `plutus-playground-client`)
      ''
        cd ${src}
        export NODE_PATH="${nodeModules}/lib/node_modules"
        ${nodejs}/bin/node -e 'require("${project}/output/${testMain}").main()'
        touch $out
      '';

  bundlePursProject =
    { name ? "${projectName}-bundle-" +
        (if browserRuntime then "web" else "nodejs")
    , entrypoint ? "index.js"
    , htmlTemplate ? "index.html"
    , main ? "Main"
    , browserRuntime ? true
    , webpackConfig ? "webpack.config.js"
    , bundledModuleName ? "output.js"
    , nodeModules ? projectNodeModules
    , ...
    }: pkgs.stdenv.mkDerivation {
      inherit name src;
      buildInputs = [
        nodejs
        nodeModules
        compiledProject
      ];
      nativeBuildInputs = [
        purs
        pkgs.easy-ps.spago
      ];
      buildPhase = ''
        export HOME="$TMP"
        export NODE_PATH="${nodeModules}/lib/node_modules"
        export PATH="${nodeModules}/bin:$PATH"
        ${pkgs.lib.optionalString browserRuntime "export BROWSER_RUNTIME=1"}
        cp -r ${compiledProject}/output .
        chmod -R +rwx .
        spago bundle-module --no-install --no-build -m "${main}" \
          --to ${bundledModuleName}
        cp $src/${entrypoint} .
        cp $src/${htmlTemplate} .
        cp $src/${webpackConfig} .
        mkdir ./dist
        webpack --mode=production -c ${webpackConfig} -o ./dist \
          --entry ./${entrypoint}
      '';
      installPhase = ''
        mkdir $out
        mv dist $out
      '';
    };

  buildPursDocsSearch =
    { name ? "purescript-docs-search"
    , ...
    }:
    let
      docsUrl =
        "https://github.com/purescript/purescript-docs-search/releases/download";
      docsVersion = "v0.0.11";
    in
    pkgs.stdenv.mkDerivation {
      inherit name;
      srcs = [
        (pkgs.fetchurl {
          url = "${docsUrl}/${docsVersion}/docs-search-app.js";
          sha256 = "17qngsdxfg96cka1cgrl3zdrpal8ll6vyhhnazqm4hwj16ywjm02";
        })
        (pkgs.fetchurl {
          url = "${docsUrl}/${docsVersion}/purescript-docs-search";
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
        cp -r ${buildPursDocs { }}/{generated-docs,output} .
        install-spago-style
        chmod -R +rwx .
        ${buildPursDocsSearch { }}/bin/purescript-docs-search build-index \
          --package-name cardano-transaction-lib
      '';
      installPhase = ''
        mkdir $out
        cp -r generated-docs $out
      '';
    };

in
{
  inherit buildPursProject runPursTest buildPursDocs bundlePursProject;
  inherit buildSearchablePursDocs buildPursDocsSearch;
  inherit purs nodejs mkNodeModules;
  devShell = shellFor shell;
}
