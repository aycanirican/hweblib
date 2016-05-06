{ hweblibSrc ? { outPath = ./.; revCount = 1234; gitTag = "abcdef"; }
, officialRelease ? false
}:

let
  pkgs = import <nixpkgs> {};
  genAttrs = (import <nixpkgs> { }).lib.genAttrs;
  genBuild = system: compiler: path: gitSource:
    let pkgs = import <nixpkgs> { inherit system; };
        haskellPackages = pkgs.lib.getAttrFromPath ["haskell-ng" "packages" compiler] pkgs;
    in
  haskellPackages.callPackage path {
  mkDerivation = expr: haskellPackages.mkDerivation (expr // {
  src = gitSource;
  version = gitSource.gitTag;
  });
  };
  
  systems = ["x86_64-linux" "i686-linux" "x86_64-darwin"];
  compilers = ["ghc784"];

  jobs = rec {
    build = pkgs.lib.genAttrs systems (system:
    
      with import <nixpkgs> { inherit system; };
      haskellPackages = pkgs.lib.getAttrFromPath ["haskell-ng" "packages" ghcVer] pkgs;
            
            
    );
  }

in jobs
