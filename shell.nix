{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.emacs
    (pkgs.haskellPackages.ghcWithPackages (p: [p.cabal-install]))
  ];
}
