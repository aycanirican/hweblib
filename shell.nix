# nix-shell --pure --arg nixpkgs /home/fxr/nixos/nixpkgs
{ nixpkgs ? <nixpkgs> }:
let
  pkgs = import nixpkgs {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      hweblib = self.callPackage ./. {};
    };
  };
in pkgs.myEnvFun {
     name = haskellPackages.hweblib.name;
     buildInputs = [
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
       ] ++ hs.hweblib.propagatedNativeBuildInputs)))
     ];
   }
