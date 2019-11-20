{ mkDerivation
, abstract-par
, base
, containers
, deepseq
, directory
, dlist
, filepath
, megaparsec
, monad-par
, monad-par-extras
, monoidal-containers
, mtl
, nonempty-containers
, optparse-applicative
, process
, stdenv
, text
, transformers
}:
mkDerivation {
  pname = "ch-hs-imports";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
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
  ];
  license = stdenv.lib.licenses.mit;
}
