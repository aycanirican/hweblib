{ mkDerivation, attoparsec, base, bytestring, containers, HUnit
, mtl, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "hweblib";
  version = "0.6.3";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring containers mtl text time transformers
  ];
  testHaskellDepends = [
    attoparsec base bytestring containers HUnit mtl time transformers
  ];
  homepage = "http://github.com/aycanirican/hweblib";
  description = "Haskell Web Library";
  license = stdenv.lib.licenses.bsd3;
}
