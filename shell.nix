{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, bytestring
      , case-insensitive, containers, doctest, Glob, HUnit, mime, mtl
      , pretty-simple, scientific, stdenv, tagsoup, text, time
      , transformers, cabal-install
      }:
      mkDerivation {
        pname = "hweblib";
        version = "0.8.0";
        src = ./.;
        libraryHaskellDepends = [
          attoparsec base bytestring case-insensitive containers mime mtl
          pretty-simple scientific tagsoup text time transformers  cabal-install
        ];
        testHaskellDepends = [
          attoparsec base bytestring containers doctest Glob HUnit mtl
          scientific text time transformers
        ];
        homepage = "http://github.com/aycanirican/hweblib";
        description = "Haskell Web Library";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
