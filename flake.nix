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
    , ...
    }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];
      perSystem = nixpkgs.lib.genAttrs defaultSystems;
      nixpkgsFor = system: import nixpkgs {
        inherit system;
      };
      psProjectFor = system:
        let
          pkgs = nixpkgsFor system;
          src = ./.;
        in
        import ./nix/purescript {
          inherit src pkgs inputs system self;
        };
    in
    {
      devShell = perSystem (system: (psProjectFor system).devShell);
      packages = perSystem (system: (psProjectFor system).packages);
      defaultPackage = perSystem (system: (psProjectFor system).defaultPackage);
      checks = perSystem (system: (psProjectFor system).checks);
      # TODO add check back
    };
}
