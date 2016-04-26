{ mkDerivation, attoparsec, base, bytestring, containers, doctest
, Glob, HUnit, mtl, scientific, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "hweblib";
  version = "0.7.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring containers mtl scientific text time
    transformers
  ];
  testHaskellDepends = [
    attoparsec base bytestring containers doctest Glob HUnit mtl
    scientific time transformers
  ];
  homepage = "http://github.com/aycanirican/hweblib";
  description = "Haskell Web Library";
  license = stdenv.lib.licenses.bsd3;
}
