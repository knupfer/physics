{ nixpkgs ? import <nixpkgs> {}, compiler ? "" }:

with nixpkgs;
let ghc = if compiler == "" then haskellPackages else haskell.packages.${compiler}; in
haskell.lib.shellAware (haskell.lib.doBenchmark (ghc.callCabal2nix "physics" (lib.sourceFilesBySuffices ./. [".cabal" ".hs" "LICENSE" ".md"]) {}))


