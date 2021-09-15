{ nixpkgs ? import ./nix/pinned.nix {} }:
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
    haskellPackages.ghcid
    pkgs.libpqxx
    pkgs.postgresql
    pkgs.zlib
  ];
  LC_ALL = "C.UTF-8";
}
