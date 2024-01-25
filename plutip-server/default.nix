{ pkgs, system, src, plutip, CHaP, cardano-node }:
let
  haskellModules =
    plutip.haskellModules.${system} ++ [
      ({ config, pkgs, ... }:
        let
          nodeExes = cardano-node.packages.${system};
        in
        {
          packages.plutip-server.components.exes.plutip-server = {
            pkgconfig = [ [ pkgs.makeWrapper ] ];
            postInstall = with pkgs; ''
              wrapProgram $out/bin/plutip-server \
                --prefix PATH : "${lib.makeBinPath [
                  nodeExes.cardano-node
                  nodeExes.cardano-cli
                ]}"
            '';
          };
        })
    ];
in
pkgs.haskell-nix.cabalProject {
  name = "plutip-server";

  inherit src;

  inputMap = {
    "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
  };

  compiler-nix-name = "ghc8107";

  shell = {
    withHoogle = true;
    exactDeps = true;

    tools.haskell-language-server = "1.5.0.0"; # Newer versions failed to build

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

      # Cardano tools
      cardano-node.packages.${system}.cardano-cli
      cardano-node.packages.${system}.cardano-node
    ];
  };

  modules = haskellModules;
}
