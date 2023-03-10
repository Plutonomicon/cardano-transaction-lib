{ pkgs, system, src, plutip, iohk-nix, CHaP }:
let
  haskellModules = plutip.haskellModules ++ [
    ({ config, pkgs, ... }: {
      packages.plutip-server.components.exes.plutip-server = {
        pkgconfig = [ [ pkgs.makeWrapper ] ];
        postInstall = with pkgs; ''
          wrapProgram $out/bin/plutip-server \
            --prefix PATH : "${lib.makeBinPath [
              config.hsPkgs.cardano-cli.components.exes.cardano-cli
              config.hsPkgs.cardano-node.components.exes.cardano-node
            ]}"
        '';
      };
    })
  ];
  extraSources = plutip.extraSources ++ [
    {
      src = plutip;
      subdirs = [ "." ];
    }
  ];
  project = pkgs.haskell-nix.cabalProject {
    name = "plutip-server";

    inherit src;

    inputMap = {
      "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
    };

    compiler-nix-name = "ghc8107";

    shell = {
      withHoogle = true;
      exactDeps = true;

      packages = ps: [ ps.plutip-server ];

      tools.haskell-language-server = "latest";

      nativeBuildInputs = with pkgs; [
        # Haskell Tools
        haskellPackages.fourmolu
        haskellPackages.cabal-install
        haskellPackages.cabal-fmt
        nixpkgs-fmt
        hlint
        entr
        ghcid
        git
        fd

        # hls doesn't support preprocessors yet so this has to exist in PATH
        haskellPackages.record-dot-preprocessor

        # Cardano tools
        project.hsPkgs.cardano-cli.components.exes.cardano-cli
        project.hsPkgs.cardano-node.components.exes.cardano-node
      ];
    };

    # inherit (bot-plutus-interface);
    inherit extraSources;
    modules = haskellModules;
  };
in
project
