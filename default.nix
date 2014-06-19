{ hs ? (import <nixpkgs> { }).haskellPackages }:

let
    inherit (hs) cabal;
in cabal.mkDerivation (self : rec {
    pname = "hweblib";
    version = "0.6.3";
    isExecutable = false;
    isLibrary = true;
    preConfigure = ''rm -rf dist'';
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
