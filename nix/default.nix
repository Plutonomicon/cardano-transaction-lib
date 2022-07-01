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
          pkgs.easy-ps.psa
          pkgs.easy-ps.spago2nix
          pkgs.nodePackages.node2nix
        ] ++ pkgs.lib.lists.optional
          pursls
          pkgs.easy-ps.purescript-language-server;
        inherit packages inputsFrom;
        shellHook =
          let
            nodeModules = mkNodeModules { };
            ogmiosFixtures = buildOgmiosFixtures { };
          in
          ''
            export NODE_PATH="${nodeModules}/lib/node_modules"
            export PATH="${nodeModules}/bin:$PATH"
          ''
          + shellHook;
      };

  buildPursProject =
    { sources ? [ "src" ]
    , withDevDeps ? false
    , name ? projectName
    , censorCodes ? [ "UserDefinedWarning" ]
    , ...
    }:
    let
      nodeModules = mkNodeModules { inherit withDevDeps; };
      sepWithComma = builtins.concatStringsSep ",";
      # for e.g. `cp -r {a,b,c}` vs `cp -r a`
      srcsStr =
        let
          withCommas = sepWithComma sources;
        in
        if builtins.length sources > 1
        then ("{" + withCommas + "}")
        else withCommas;
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
        spagoPkgs.installSpagoStyle
        pkgs.easy-ps.psa
      ];
      nativeBuildInputs = [
        purs
        pkgs.easy-ps.spago
      ];
      unpackPhase =
        ''
          export HOME="$TMP"
          export NODE_PATH="${nodeModules}/lib/node_modules"
          export PATH="${nodeModules}/bin:$PATH"
          cp -r $src/${srcsStr} .
          install-spago-style
        '';
      buildPhase = ''
        psa --strict --censor-lib --is-lib=.spago ${spagoGlobs} \
          --censor-codes=${sepWithComma censorCodes} "./**/*.purs"
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
        buildPhase = oas.buildPhase + ''
          ${pkgs.lib.optionalString browserRuntime "export BROWSER_RUNTIME=1"}
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
      });

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

  buildOgmiosFixtures =
    { name ? "${projectName}-ogmios-fixtures"
    }@args:
    pkgs.stdenv.mkDerivation {
      inherit name;
      dontUnpack = true;
      buildInputs = [ pkgs.jq ];
      buildPhase = ''
        cp -r ${pkgs.ogmios-fixtures}/server/test/vectors/StateQuery/Response .
        chmod -R +rwx .

        function on_file () {
          local query_regex='.*Query\[(.*)\].*'
          if [[ "$1" =~ $query_regex ]]
          then
            echo "$1"
            json=$(jq -c .result "$1")
            md5=($(md5sum <<< $json))
            printf "%s" "$json" > "ogmios/''${BASH_REMATCH[1]}-''${md5}.json"
          fi
        }
        export -f on_file

        mkdir ogmios
        find . -type f -name "*.json" -exec bash -c 'on_file "{}"' \;
      '';
      installPhase = ''
        mkdir $out
        cp -rT ogmios $out
      '';
    };

in
{
  inherit buildPursProject runPursTest buildPursDocs bundlePursProject;
  inherit buildSearchablePursDocs buildPursDocsSearch;
  inherit buildOgmiosFixtures;
  inherit purs nodejs mkNodeModules;
  devShell = shellFor shell;
}
