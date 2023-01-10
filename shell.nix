with (import <unstable> {});
let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/ee01de29d2f58d56b1be4ae24c24bd91c5380cea.tar.gz";
  }) {};
in
mkShell {
  buildInputs = [
    (haskell.packages.ghc90.ghcWithPackages (p: with p; [
      pkgs.haskellPackages.clash-ghc
      ghc-typelits-extra
      ghc-typelits-knownnat
      ghc-typelits-natnormalise
    ])
)
  ];
}
