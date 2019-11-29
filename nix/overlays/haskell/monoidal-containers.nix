{ mkDerivation, aeson, base, containers, deepseq, hashable, lens
, newtype, semialign, semigroups, stdenv, these
, unordered-containers
}:
mkDerivation {
  pname = "monoidal-containers";
  version = "0.6.0.1";
  sha256 = "02c9c21422bc2557bdea63854991604eab39289114125cb51c936fed8176b5c8";
  libraryHaskellDepends = [
    aeson base containers deepseq hashable lens newtype semialign
    semigroups these unordered-containers
  ];
  homepage = "http://github.com/bgamari/monoidal-containers";
  description = "Containers with monoidal accumulation";
  license = stdenv.lib.licenses.bsd3;
}
