{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, bytestring, containers
      , HUnit, mtl, stdenv, text, time, transformers
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
