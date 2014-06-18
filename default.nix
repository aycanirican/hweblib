{ nixpkgs ? import <nixpkgs> { }, hs ? nixpkgs.haskellPackages }:
with nixpkgs;

let
    inherit (hs) cabal;
in

cabal.mkDerivation (self : rec {
    pname = "hweblib";
    version = "0.6.3";
    isExecutable = false;
    isLibrary = true;
    src = ./.;

    buildDepends = [
        hs.attoparsec
        hs.mtl
        hs.text
        hs.transformers
    ];

    testDepends = [
        hs.attoparsec
        hs.HUnit
        hs.mtl
        hs.transformers
    ];
})
