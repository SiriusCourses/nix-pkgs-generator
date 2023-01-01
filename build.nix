let
  cheops = import ./default.nix;
  fetch = import ./nix/fetch.nix;
  pkgs = import (fetch "nixpkgs") {
    config = { allowUnfree = true; enableParallelBuilding = true;  allowBroken = true; };
    overlays = [
      (local.mk-overlay {})
    ];
  };
in pkgs.cheopslab.haskellPackages.ghcWithPackages (p: with p; [
   # ...
])
