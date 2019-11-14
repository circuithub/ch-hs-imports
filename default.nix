{ mkDerivation, base, containers, directory, dlist, filepath
, megaparsec, optparse-applicative, stdenv, streaming
, streaming-bytestring, streaming-process, text, transformers
}:
mkDerivation {
  pname = "ch-hs-imports";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers directory dlist filepath megaparsec
    optparse-applicative streaming streaming-bytestring
    streaming-process text transformers
  ];
  license = stdenv.lib.licenses.mit;
}
