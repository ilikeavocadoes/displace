{ nixpkgs ? import ./nixpkgs.nix }:

with nixpkgs;

haskellPackages.shellFor {
  packages = p: with p; [
    (callPackage ./default.nix {})
  ];
  buildInputs = with haskellPackages; [
    yaml
    text
    containers
    regex-tdfa
    regex-tdfa-text
  ];
}
