{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
in
pkgs.mkShell {
  name = "real-world-yesod";
  buildInputs = [
    haskellPackages.stack
    haskellPackages.ghc
    haskellPackages.yesod-bin
    pkgs.libpqxx
  ];
}
