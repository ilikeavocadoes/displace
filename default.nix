{ nixpkgs ? import ./nixpkgs.nix }:

with nixpkgs;

let displace = haskellPackages.callCabal2nix "displace" ./. {};
in displace
