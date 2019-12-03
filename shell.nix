let
  name = "ch-hs-imports";
  # the pegged and customized nix package set
  pkgs = import ./nix/pkgs {};
  ghc = pkgs.haskellPackages.ghcWithHoogle
    (
      hs: with hs; [
        # cabal dependencies
        abstract-par
        base
        containers
        deepseq
        directory
        dlist
        filepath
        megaparsec
        monad-par
        monad-par-extras
        monoidal-containers
        mtl
        nonempty-containers
        optparse-applicative
        process
        text
        transformers
        # extra useful dependencies
        pretty-show
        #rio
      ]
    );
  # the dependencies available in the shell
  dependencies = with pkgs;[
    # tools
    ghc
    cabal-install
  ];
  # fake package explicitly adding dependencies to all the shell dependencies for adding GC roots
  pathToDependencies = pkgs.runCommand "build" {
    name = "${name}-pathToDependencies";
    # add explicit dependencies to the path of the instantiated nixpkgs and also
    # the pegged source of nixpkgs
    paths = dependencies ++ [ pkgs.nixpkgs-src pkgs.path ];
  } "echo $paths > $out";

  # create the non buildable shell only environment
in
pkgs.mkShell {

  name = "${name}-shell";

  buildInputs = dependencies;

  # perform some setup on entering the shell
  shellHook = ''

    # set NIX_PATH to point to the pegged nix packages
    export NIX_PATH="nixpkgs=${pkgs.nixpkgs-src}";

    # setup environment to find the correct GHC package set
    NIX_GHC=$(type -p ghc)
    if [ -n "$NIX_GHC" ]; then
      eval $(grep export "$NIX_GHC")
    fi

    # add GC roots so that nix-collect-garbage does not delete our environment
    mkdir -p ./.gcroots
    # GC root to the actual shell file
    ${pkgs.nix}/bin/nix-instantiate ./shell.nix --indirect --add-root "./.gcroots/$(basename $out)-shell.drv" > /dev/null
    # GC root to the fake package which keeps all our dependencies alive
    ${pkgs.nix}/bin/nix-store --add-root ./.gcroots/$(basename ${pathToDependencies}) --indirect --realise ${pathToDependencies} > /dev/null
  '';
}
