(
  let
    pkgs = import <nixpkgs> {};
    lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  in rec {
    flake-compat = fetchTarball {
        url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
        sha256 = lock.nodes.flake-compat.locked.narHash;
    };
    shellNix = (import flake-compat { src = ./.; }).shellNix;
    ogmios-datum-cache = pkgs.haskellPackages.callPackage (
      fetchTarball {
        url = "https://github.com/mlabs-haskell/ogmios-datum-cache/archive/${lock.nodes.ogmios-datum-cache.locked.rev}.tar.gz";
        sha256 = lock.nodes.ogmios-datum-cache.locked.narHash;
      }) {};
    shellNixDefault = shellNix.default.overrideAttrs(old: {buildInputs = old.buildInputs ++ [ogmios-datum-cache];});}).shellNixDefault
