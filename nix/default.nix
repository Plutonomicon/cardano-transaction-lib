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
            export NODE_PATH="${nodeModules}/node_modules"
            export PATH="${nodeModules}/bin:$PATH"
          ''
          + shellHook;
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
          cp -r ${nodeModules}/lib/node_modules .
          chmod -R u+rw node_modules
          export NODE_PATH="$PWD/node_modules:$NODE_PATH"
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
          webpack --mode=production -c ${webpackConfig} -o ./dist
        '';
        installPhase = ''
          mkdir $out
          mv dist $out
        '';
      });

in
{
  inherit buildPursProject runPursTest bundlePursProject;
  inherit purs nodejs mkNodeModules;
  devShell = shellFor shell;
}
