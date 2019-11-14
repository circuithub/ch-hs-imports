{ mkDerivation, base, megaparsec, optparse-applicative, stdenv
, streaming, streaming-bytestring, streaming-process, text
}:
mkDerivation {
  pname = "ch-hs-imports";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base megaparsec optparse-applicative streaming streaming-bytestring
    streaming-process text
  ];
  license = stdenv.lib.licenses.mit;
}
