{ pkgs ? import <nixpkgs> {},
  ...
}:

with pkgs;
with haskellPackages;

callPackage ./pkg.nix { inherit (haskell.lib) dontHaddock; }
