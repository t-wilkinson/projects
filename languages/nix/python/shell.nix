with import <nixpkgs> {};

let
  python3Env = pkgs.python36.withPackages (ps: with ps; [
    # qiskit
    numpy
    # pandas
    # mnist
    # Keras
    pyglet
    # tensorflow
    # tensorflowWithCuda
  ]);
  # development = [ neovim ];
  # terminal = [ zsh neofetch ];
in
pkgs.mkShell rec {
  # shellHook = "zsh";
  buildInputs = [
    python3Env
  ]
  # ++ development
  # ++ terminal;
  ;
}
