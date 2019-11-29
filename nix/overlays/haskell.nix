self: super:
let
  haskell = super.haskell // {
    packages = builtins.mapAttrs (
      name: value:
        let
          selfPkgs = self;
          superPkgs = super;
        in
          value.override {
            overrides =
              with superPkgs.lib.attrsets;
              with superPkgs.haskell.lib;
              self: super: {

                monoidal-containers = self.callPackage ./haskell/monoidal-containers.nix {};
                streaming-process = doJailbreak (self.callPackage ./haskell/streaming-process.nix {});

              };
          }
    ) super.haskell.packages;
  };
in
{ inherit haskell; }
