with (import <nixpkgs> {});
mkShell rec {
  buildInputs = [ gcc ];
}
