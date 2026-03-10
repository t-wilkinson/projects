{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    esp-idf.url = "github:mirrexagon/nixpkgs-esp-dev";
    nixpkgs-mozilla.url = "github:mozilla/nixpkgs-mozilla";
  };
  outputs =
    {
      self,
      nixpkgs,
      ...
    }@inputs:
    {
      devShells.x86_64-linux =
        let
          system = "x86_64-linux";
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              inputs.nixpkgs-mozilla.overlays.rust
              inputs.esp-idf.overlays.default
            ];
          };
          rust-channel = pkgs.rustChannelOf {
            date = "2025-03-09";
            channel = "nightly";
            sha256 = "sha256-U3FFtPBXG640f0fN0ldSd7A3Vq8x213KzQu6Ns/snGo=";
          };
          rustnightly = (
            rust-channel.rust.override {
              extensions = [ "rust-src" ];
            }
          );
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              #esp-idf-full
              (esp-idf-esp32c6.override {
                #rev = "v5.3.2";
                #sha256 = "sha256-sQYylDGl7tDQzLOee3yw+Ev+oJzCyJQ7cNDXWaDkUTk=";
                rev = "v5.1.6";
                sha256 = "sha256-Zeew4Ekp6jW+kx7MOTnG3cvEqIDF1v+yOCf5uuQmUkM=";
              })
              #cargo
              #cargo-generate
              rustnightly
              #ldproxy
              #espflash
            ];
            LIBCLANG_PATH = "${pkgs.libclang.lib}/lib";
          };
        };
    };
}
