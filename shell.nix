let
  fetch = import ./nix/fetch.nix;
  pkgs = import (fetch "nixpkgs") {};
  ghcWithPackages = pkgs.haskellPackages.ghcWithPackages (p: with p; [
    shake
    yaml 
    PyF 
    cabal2nix 
    cabal-install
  ]);
in
  pkgs.mkShell {
    nativeBuildInputs = with pkgs; [
        ghcWithPackages
        colordiff
        nix-prefetch-git
      ];
}
