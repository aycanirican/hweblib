{ mkDerivation, attoparsec, base, bytestring, case-insensitive
, containers, doctest, Glob, HUnit, mtl, pretty-simple, scientific
, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "hweblib";
  version = "0.8.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring case-insensitive containers mtl
    pretty-simple scientific text time transformers
  ];
  testHaskellDepends = [
    attoparsec base bytestring containers doctest Glob HUnit mtl
    scientific text time transformers
  ];
  homepage = "http://github.com/aycanirican/hweblib";
  description = "Haskell Web Library";
  license = stdenv.lib.licenses.bsd3;
}
