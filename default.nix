{ mkDerivation
, async
, base
, containers
, directory
, dlist
, filepath
, megaparsec
, monoidal-containers
, nonempty-containers
, optparse-applicative
, stdenv
, streaming
, streaming-bytestring
, streaming-process
, text
, transformers
, unliftio-core
}:
mkDerivation {
  pname = "ch-hs-imports";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async
    base
    containers
    directory
    dlist
    filepath
    megaparsec
    monoidal-containers
    nonempty-containers
    optparse-applicative
    streaming
    streaming-bytestring
    streaming-process
    text
    transformers
    unliftio-core
  ];
  license = stdenv.lib.licenses.mit;
}
