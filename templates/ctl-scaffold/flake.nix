{
  description = "A nix flake for ctl-scaffold example.";

  nixConfig = {
    extra-experimental-features = [ "flakes" "nix-command" ];
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]ctl-scaffold@\\[\\033[33m\\]$(git rev-parse --abbrev-ref HEAD) \\[\\e[0;32m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  };

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    liqwid-nix.url = "github:mlabs-haskell/liqwid-nix/aciceri/fix-new-ctl";
    cardano-transaction-lib.url = "github:Plutonomicon/cardano-transaction-lib/63485e328b6008b5bea336269b9d6036d04c7c7b";
    # Needed for liqwid-nix
    nixpkgs-ctl.follows = "cardano-transaction-lib/nixpkgs";
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } ({ self, ... }: {
    imports = [ inputs.liqwid-nix.flakeModule ];
    systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
    perSystem = { system, lib, pkgs, ... }: {
      offchain.default = {
        src = builtins.path {
          path = self.outPath;
          name = "ctl-scaffold-filtered-src";
          filter = path: ftype: !(lib.hasSuffix ".md" path) && (builtins.baseNameOf path != "flake.nix");
        };

        nodejsPackage = pkgs.nodejs-18_x;
        packageLock = ./package-lock.json;
        packageJson = ./package.json;

        ignoredWarningCodes = [
          "ImplicitImport"
          "ImplicitQualifiedImport"
          "ImplicitQualifiedImportReExport"
          "UserDefinedWarning"
        ];

        shell = {
          # shellHook = ''
          # '';
          extraCommandLineTools = [
          ];
        };

        enableFormatCheck = true;
      };

      pre-commit.settings.hooks.nixpkgs-fmt = {
        enable = true;
        excludes = [ "spago-packages.nix" ];
      };
    };
  });
}
