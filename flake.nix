{
  description = "ch-hs-imports";

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };

  inputs = {
    haskellNix.url = github:input-output-hk/haskell.nix;
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [ haskellNix.overlay ];
        };

        ch-hs-imports = pkgs.haskell-nix.project {
          compiler-nix-name = "ghc982";

          src = pkgs.haskell-nix.haskellLib.cleanGit {
            name = "ch-hs-imports";
            src = ./.;
          }; 
        };
      in
      {
        packages.default = ch-hs-imports.hsPkgs.ch-hs-imports.components.exes.ch-hs-imports;
      }
    );
}
