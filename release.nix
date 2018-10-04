{ nixpkgs ? import ./nixpkgs.nix }:

with nixpkgs.haskell.lib;
with nixpkgs.lib;

let tested = [ "ghc7103" "ghc802" "ghc822" "ghc843" "ghc861" "ghcHEAD" ];
    eval = x: import ./default.nix { nixpkgs = nixpkgs; compiler = x; };

in
{ sdist = sdistTarball (eval (last tested));
} // genAttrs tested eval
