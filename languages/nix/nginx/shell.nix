{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let
  nginx-test = import ./. {};
in mkShell {
  buildInputs = [ nginx-test ];
}
