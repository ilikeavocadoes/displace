{ nixpkgs ? import ./nixpkgs.nix }:

with nixpkgs;

let displace = callPackage ./derivation.nix {};
in displace
