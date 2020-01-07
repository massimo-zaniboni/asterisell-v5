{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
  inherit (pkgs) rPackages;

in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = [
    pkgs.netbeans
    pkgs.php
    pkgs.php72Packages.phpcs
    pkgs.csvkit
    pkgs.graphviz
    pkgs.R
    rPackages.tidyverse
    rPackages.DiagrammeR
    rPackages.rmarkdown
    rPackages.testit
  ];
}
