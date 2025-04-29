let
  pkgs = import <nixpkgs> {};
  mkDerivation = import ./auto.nix pkgs;
in mkDerivation {
  name = "hello";
  src = ./hello-2.10.tar.gz;
}
