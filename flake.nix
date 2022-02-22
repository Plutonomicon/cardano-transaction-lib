{
  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # for the purescript project
    ogmios.url = "github:mlabs-haskell/ogmios";
    ogmios-datum-cache = {
      url = "github:mlabs-haskell/ogmios-datum-cache";
      flake = false;
    };
    # so named because we also need a different version of the repo below
    # in the server inputs and we use this one just for the `cardano-cli`
    # executables
    cardano-node-exe = {
      url = "github:input-output-hk/cardano-node/ea8b632820db5546b22430bbb5ed8db4a2fef7dd";
    };
    cardano-configurations = {
      url = "github:input-output-hk/cardano-configurations";
      flake = false;
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };

    # for the haskell server
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    haskell-nix.url = "github:L-as/haskell.nix?ref=master";
    nixpkgs.follows = "haskell-nix/nixpkgs-2105";
    nixpkgs-unstable.follows = "haskell-nix/nixpkgs-unstable";
    cardano-addresses = {
      url =
        "github:input-output-hk/cardano-addresses/d2f86caa085402a953920c6714a0de6a50b655ec";
      flake = false;
    };
    cardano-base = {
      url =
        "github:input-output-hk/cardano-base/4ea7e2d927c9a7f78ddc69738409a5827ab66b98";
      flake = false;
    };
    cardano-crypto = {
      url =
        "github:input-output-hk/cardano-crypto/07397f0e50da97eaa0575d93bee7ac4b2b2576ec";
      flake = false;
    };
    cardano-ledger-specs = {
      url =
        "github:input-output-hk/cardano-ledger-specs/bf008ce028751cae9fb0b53c3bef20f07c06e333";
      flake = false;
    };
    cardano-node = {
      url =
        "github:input-output-hk/cardano-node/2cbe363874d0261bc62f52185cf23ed492cf4859";
      flake = false;
    };
    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/fd773f7a58412131512b9f694ab95653ac430852";
      flake = false;
    };
    cardano-wallet = {
      url =
        "github:j-mueller/cardano-wallet/6be73ab852c0592713dfe78218856d4a8a0ee69e";
      flake = false;
    };
    flat = {
      url =
        "github:input-output-hk/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
    goblins = {
      url =
        "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };
    iohk-monitoring-framework = {
      url =
        "github:input-output-hk/iohk-monitoring-framework/46f994e216a1f8b36fe4669b47b2a7011b0e153c";
      flake = false;
    };
    optparse-applicative = {
      url =
        "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };
    ouroboros-network = {
      url =
        "github:input-output-hk/ouroboros-network/1f4973f36f689d6da75b5d351fb124d66ef1057d";
      flake = false;
    };
    plutus = {
      url =
        "github:input-output-hk/plutus/3f089ccf0ca746b399c99afe51e063b0640af547";
      flake = false;
    };
    # NOTE
    # I don't we need anything from `plutus-apps`, so the following two are
    # not necessary. If we go with Servant for the server, though, they might
    # be useful for communicating with the frontend
    purescript-bridge = {
      url =
        "github:input-output-hk/purescript-bridge/366fc70b341e2633f3ad0158a577d52e1cd2b138";
      flake = false;
    };
    servant-purescript = {
      url =
        "github:input-output-hk/servant-purescript/ebea59c7bdfc0338d83fca772b9a57e28560bcde";
      flake = false;
    };
    Win32-network = {
      url =
        "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, iohk-nix, ... }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];
      perSystem = nixpkgs.lib.genAttrs defaultSystems;
      nixpkgsFor = system: import nixpkgs {
        overlays = [
          haskell-nix.overlay
          iohk-nix.overlays.crypto
        ];
        inherit (haskell-nix) config;
        inherit system;
      };
      psProjectFor = system:
        let
          pkgs = nixpkgsFor system;
          src = ./.;
        in
        import ./nix {
          inherit src pkgs inputs system self;
        };
      hsProjectFor = system:
        let
          pkgs = nixpkgsFor system;
          src = ./server;
        in
        import ./server/nix {
          inherit src inputs pkgs system;
        };
    in
    {
      # flake from haskell.nix project
      hsFlake = perSystem (system: (hsProjectFor system).flake { });

      devShell = perSystem (system: (psProjectFor system).devShell);

      # It might be a good idea to keep this as a separate shell; if you're
      # working on the PS frontend, it doesn't make a lot of sense to pull
      # in all of the Haskell dependencies
      #
      # This can be used with `nix develop .#hsDevShell.<SYSTEM>`
      hsDevShell = perSystem (system: self.hsFlake.${system}.devShell);

      packages = perSystem (system:
        self.hsFlake.${system}.packages // (psProjectFor system).packages
      );

      apps = perSystem (system: {
        inherit
          (self.hsFlake.${system}.apps)
          "cardano-browser-tx-server:exe:cardano-browser-tx-server";
      });

      defaultPackage = perSystem (system: (psProjectFor system).defaultPackage);

      checks = perSystem (system: (psProjectFor system).checks);

      check = perSystem (system: (psProjectFor system).check);
    };
}
