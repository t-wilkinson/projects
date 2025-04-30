{ pkgs ? import <nixpkgs> {} }:
with pkgs;
stdenv.mkDerivation {
  name = "nginx-test-shell";
  builder = "${bash}/bin/bash";
  args = [ ./builder.sh ];
  buildInputs = [ coreutils ];
  nginxCmd = ''${pkgs.nginx}/bin/nginx -c ${./nginx.conf} "$@"'';
  system = builtins.currentSystem;
}


# { pkgs ? import <nixpkgs> {} }:
# with pkgs;
# let
#   nginx-with-config = pkgs.writeScriptBin "nginx" ''
#     exec ${pkgs.nginx}/bin/nginx -c ${./nginx.conf} "$@"
#   '';
# in mkShell {
#   name = "my-shell";
#   buildInputs = [
#     nginx-with-config
#   ];
# }
