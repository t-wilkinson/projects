{
  description = "ESP32-CAM Hybrid Object Detection Environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    esp-rs-nix.url = "github:newAM/esp-rs-nix";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs =
    {
      self,
      nixpkgs,
      esp-rs-nix,
      rust-overlay,
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ rust-overlay.overlays.default ];
      };

      # Access the specific ESP32 packages from the input flake
      esp-pkgs = esp-rs-nix.packages.${system};
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          # ESP tools
          esp-pkgs.espflash
          esp-pkgs.espmonitor
          # esp-pkgs.rustc # Xtensa-enabled rustc
          # esp-pkgs.cargo # Compatible cargo
          esp-pkgs.llvm-xtensa # Required for linking/bindgen

          (pkgs.rust-bin.stable.latest.default.override {
            extensions = [
              "rust-src"
              "rust-analysis"
            ];
            # If you need Xtensa specifically for ESP32/S3/S2:
            # targets = [ "xtensa-esp32-none-elf" ];
          })

          # Rust tools
          pkgs.espup
          pkgs.ldproxy
          pkgs.cargo-generate
          # pkgs.espflash
          # pkgs.cargo

          # Python
          (pkgs.python3.withPackages (
            ps: with ps; [
              ultralytics # YOLO26-M
              albumentations # Preprocessing
              opencv4
              jupyter
              websockets
              pycocotools
              matplotlib
            ]
          ))

          # Build tools
          pkgs.pkg-config
          pkgs.openssl
          pkgs.libusb1
        ];

        # Required environment variables for ESP32 Rust builds
        LIBCLANG_PATH = "${esp-pkgs.llvm-xtensa}/lib";

        shellHook = ''
          echo "Using Xtensa Rust toolchain from esp-rs-nix"
          echo "Run 'jupyter notebook' to start training."
          export LD_LIBRARY_PATH="${
            pkgs.lib.makeLibraryPath [
              pkgs.libudev-zero
              pkgs.openssl
            ]
          }"
          [ -f $HOME/export-esp.sh ] && source $HOME/export-esp.sh
        '';
      };
    };
}
