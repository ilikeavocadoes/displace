{ haskellPackages }:

let drv = haskellPackages.callCabal2nix "displace" ./. {};
in drv
