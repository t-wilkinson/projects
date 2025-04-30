{
  description = "Nix flake for setting up Python environment with numpy, pandas, and matplotlib";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          python311
          python311Packages.numpy
          python311Packages.pandas
          python311Packages.matplotlib
          stdenv.cc.cc.lib
        ];

        shellHook = ''
          if [ -f .venv/bin/activate ]; then
            source .venv/bin/activate
          fi
        '';
      };
    };
}
