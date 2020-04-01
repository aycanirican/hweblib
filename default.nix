{ mkDerivation, attoparsec, base, bytestring, case-insensitive
, containers, doctest, Glob, HUnit, mime, mtl, pretty-simple
, scientific, stdenv, tagsoup, text, time, transformers
}:
mkDerivation {
  pname = "hweblib";
  version = "0.8.3";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring case-insensitive containers mime mtl
    pretty-simple scientific tagsoup text time transformers
  ];
  testHaskellDepends = [
    attoparsec base bytestring containers doctest Glob HUnit mtl
    scientific text time transformers
  ];
  homepage = "http://github.com/aycanirican/hweblib";
  description = "Haskell Web Library";
  license = stdenv.lib.licenses.bsd3;
}
