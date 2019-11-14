{ mkDerivation, base, bytestring, directory, exceptions, hspec
, lifted-async, monad-control, process, QuickCheck
, quickcheck-instances, stdenv, streaming, streaming-bytestring
, streaming-commons, streaming-concurrency, streaming-with
, transformers, transformers-base
}:
mkDerivation {
  pname = "streaming-process";
  version = "0.1.0.0";
  sha256 = "12a80a3648f68a0700d58108d5ab511f5d97b5e1fe994d3ac79fe0fc2f221060";
  libraryHaskellDepends = [
    base bytestring directory exceptions lifted-async monad-control
    process streaming streaming-bytestring streaming-commons
    streaming-concurrency streaming-with transformers transformers-base
  ];
  testHaskellDepends = [
    base bytestring hspec QuickCheck quickcheck-instances streaming
    streaming-bytestring
  ];
  description = "Streaming support for running system process";
  license = stdenv.lib.licenses.mit;
}
