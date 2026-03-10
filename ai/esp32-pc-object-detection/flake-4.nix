{
  description = "ESP32-S Rust WebSocket server development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        # ──────────────────────────────────────────────────────────────────
        # VERSION PINS — bump these to update, then run:
        #   nix flake update        (refreshes flake.lock)
        #   nix develop             (let it fail to surface correct hashes)
        # ──────────────────────────────────────────────────────────────────

        # https://github.com/esp-rs/rust-build/releases
        xtensaRustVersion = "1.93.0.0";

        # https://github.com/espressif/crosstool-NG/releases
        xtensaGccVersion = "13.2.0_20240530";

        # https://github.com/espressif/esp-idf/releases
        espIdfVersion = "v5.3.1";

        # run `nix develop`, let it fail, copy the "got: sha256-..."
        platformAttrs = {
          "x86_64-linux" = {
            rustTriple = "x86_64-unknown-linux-gnu";
            gccTriple = "x86_64-linux-gnu";
            rustHash = "sha256-a25Gq2TJItoO3q17Oq6pY/DmvTxuT3KiTTpMKv36rWo=";
            rustSrcHash = "sha256-9pDLZWProFC03G4n9OtTxSClNaVoQ1KbaaibajqQ6Kg=";
            gccHash = "sha256-/O8D2H6sRMDb7iu+6YRD7S/PgnINzY6/4AZAgHsPB8I=";
          };
          "aarch64-linux" = {
            rustTriple = "aarch64-unknown-linux-gnu";
            gccTriple = "aarch64-linux-gnu";
            rustHash = "sha256-RUST+HASH+AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
            rustSrcHash = "sha256-RUST+SRC+HASH+aarch64+AAAAAAAAAAAAAAAAAAAAA=";
            gccHash = "sha256-GCC+HASH+aarch64+AAAAAAAAAAAAAAAAAAAAAAAAAA=";
          };
          "x86_64-darwin" = {
            rustTriple = "x86_64-apple-darwin";
            gccTriple = "x86_64-apple-darwin";
            rustHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
            rustSrcHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
            gccHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
          };
          "aarch64-darwin" = {
            rustTriple = "aarch64-apple-darwin";
            gccTriple = "aarch64-apple-darwin";
            rustHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
            rustSrcHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
            gccHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
          };
        };

        plat = platformAttrs.${system} or (throw "esp32s-rust-ws: unsupported system '${system}'");

        # ──────────────────────────────────────────────────────────────────
        # XTENSA RUST TOOLCHAIN
        #
        # Fetches two tarballs from esp-rs/rust-build:
        #   rust-<ver>-<triple>.tar.xz     compiler + cargo + std libs
        #   rust-src-<ver>.tar.xz          Rust source (required by esp-idf-sys)
        #
        # Each tarball ships an install.sh.  We call it with
        #   --destdir=$out --prefix=""
        # so all paths land under the Nix store output.
        #
        # autoPatchelfHook (Linux only) rewrites ELF RPATHs so the
        # pre-built binaries find their shared libraries in the store.
        # ──────────────────────────────────────────────────────────────────
        xtensaRust = pkgs.stdenv.mkDerivation {
          pname = "xtensa-rust";
          version = xtensaRustVersion;

          # Declare the two tarballs as named attrs so we can reference
          # them in installPhase without relying on a single $src.
          rustTarball = pkgs.fetchurl {
            url =
              "https://github.com/esp-rs/rust-build/releases/download"
              + "/v${xtensaRustVersion}"
              + "/rust-${xtensaRustVersion}-${plat.rustTriple}.tar.xz";
            hash = plat.rustHash;
          };
          rustSrcTarball = pkgs.fetchurl {
            url =
              "https://github.com/esp-rs/rust-build/releases/download"
              + "/v${xtensaRustVersion}"
              + "/rust-src-${xtensaRustVersion}.tar.xz";
            hash = plat.rustSrcHash;
          };

          # We handle unpacking ourselves in installPhase.
          dontUnpack = true;

          nativeBuildInputs = [
            pkgs.xz
            pkgs.makeWrapper
          ]
          ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.autoPatchelfHook ];

          buildInputs = pkgs.lib.optionals pkgs.stdenv.isLinux [
            pkgs.stdenv.cc.cc.lib # libstdc++.so.6
            pkgs.zlib
          ];

          installPhase = ''
            runHook preInstall

            echo ">>> Unpacking Xtensa Rust compiler..."
            tar -xf "$rustTarball"
            ls
            pushd rust-*
              bash install.sh \
                --destdir="$out" \
                --prefix=""      \
                --without=rust-docs
            popd

            echo ">>> Unpacking rust-src..."
            tar -xf "$rustSrcTarball"
            ls
            pushd rust-src-*
              bash install.sh \
                --destdir="$out" \
                --prefix=""
            popd

            runHook postInstall
          '';

          # Pre-built vendor binaries — never let Nix strip them.
          dontStrip = true;
          dontPatchELF = false; # autoPatchelfHook still runs

          meta.description = "Xtensa-patched Rust compiler for ESP32 (esp-rs fork v${xtensaRustVersion})";
        };

        # ──────────────────────────────────────────────────────────────────
        # XTENSA-ESP-ELF GCC CROSS-COMPILER
        #
        # Pre-built GCC targeting Xtensa, released by Espressif at
        # https://github.com/espressif/crosstool-NG/releases.
        # The tarball unpacks to a self-contained prefix; we copy it
        # into $out and let autoPatchelfHook fix the ELF headers.
        # ──────────────────────────────────────────────────────────────────
        xtensaEspElf = pkgs.stdenv.mkDerivation {
          pname = "xtensa-esp-elf";
          version = xtensaGccVersion;

          src = pkgs.fetchurl {
            url =
              "https://github.com/espressif/crosstool-NG/releases/download"
              + "/esp-${xtensaGccVersion}"
              + "/xtensa-esp-elf-${xtensaGccVersion}-${plat.gccTriple}.tar.xz";
            hash = plat.gccHash;
          };

          nativeBuildInputs = pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.autoPatchelfHook ];

          buildInputs = pkgs.lib.optionals pkgs.stdenv.isLinux [
            pkgs.stdenv.cc.cc.lib
            pkgs.zlib
            pkgs.python3
          ];

          # The tarball unpacks to a single directory; $sourceRoot is that
          # directory, so we just copy everything out.
          installPhase = ''
            runHook preInstall
            mkdir -p "$out"
            cp -a . "$out/"
            runHook postInstall
          '';

          dontStrip = true;

          meta.description = "Xtensa GCC cross-compiler for ESP32 (Espressif crosstool-NG ${xtensaGccVersion})";
        };

        # ──────────────────────────────────────────────────────────────────
        # ESP-IDF SOURCE TREE
        #
        # Pinned checkout of the Espressif IoT Development Framework.
        # We skip submodules here — embuild pulls the component manager
        # components it actually needs on first `cargo build`.
        #
        # To get the correct hash:
        #   nix-prefetch-github espressif esp-idf --rev v5.3.1
        # ──────────────────────────────────────────────────────────────────
        # espIdf = pkgs.fetchFromGitHub {
        #   owner = "espressif";
        #   repo = "esp-idf";
        #   rev = espIdfVersion;
        #   hash = "sha256-VG8Ow7NFePqACmJ5AzT1Ju6p5t/LbgPGfb80ufdYtx8=";
        #   fetchSubmodules = false;
        #   leaveDotGit = true; # necessary for embuild to run `git rev-parse`
        # };
        espIdf = pkgs.fetchFromGitHub {
          "owner" = "espressif";
          "repo" = "esp-idf";
          "rev" = espIdfVersion; # "c8fc5f643b7a7b0d3b182d3df610844e3dc9bd74";
          "hash" = "sha256-x/PCmaW5DODW/JxwwmxEIIZ4Gs0qgR7s7KLTBdekfdw=";
          "leaveDotGit" = true;
        };

        # LDPROXY
        # Tiny linker-proxy required by esp-idf-sys / embuild.
        ldproxy = pkgs.rustPlatform.buildRustPackage rec {
          pname = "ldproxy";
          version = "0.3.3";

          src = pkgs.fetchCrate {
            inherit pname version;
            hash = "sha256-XLfa40eMkeUL544gDqZYbly2E5Mrogn7v24D8u/wjkg=";
          };

          cargoHash = "sha256-orWs8KYFUtMp5vbwhr3O13FGXjXXKZ6Idp+ZS538P+Y=";

          meta = {
            description = "Linker proxy for esp-idf-sys / embuild";
            homepage = "https://github.com/esp-rs/embuild";
            license = pkgs.lib.licenses.mit;
          };
        };

        # ──────────────────────────────────────────────────────────────────
        # ESP-IDF PYTHON ENVIRONMENT
        #
        # embuild calls `idf_tools.py export` and requires the output to
        # contain IDF_PYTHON_ENV_PATH.  Under Nix, idf_tools.py never
        # manages a venv, so the variable is never emitted.  We sidestep
        # this by building a withPackages env ourselves and exporting it
        # directly — embuild accepts any path that contains bin/python.
        # ──────────────────────────────────────────────────────────────────
        espIdfPython = pkgs.python311.withPackages (
          ps: with ps; [
            pyserial
            click
            cryptography
            pyparsing
            pyelftools
            kconfiglib
            future
            construct
            pygdbmi
          ]
        );
      in
      {
        # ════════════════════════════════════════════════════════════════
        # DEV SHELL  —  enter with: nix develop
        # ════════════════════════════════════════════════════════════════
        devShells.default = pkgs.mkShell {
          name = "esp32s-rust-ws";

          packages = [
            espIdfPython

            # Toolchains
            xtensaRust
            xtensaEspElf

            # ESP cargo utilities
            pkgs.cargo-generate # cargo generate esp-rs/esp-idf-template
            pkgs.espflash # flash + serial monitor
            ldproxy # linker proxy (required by esp-idf-sys)

            # Build system
            pkgs.cmake
            pkgs.ninja
            pkgs.python3 # ESP-IDF Python build scripts
            pkgs.git

            # Bindgen (esp-idf-sys generates C FFI bindings at build time)
            pkgs.clang
            pkgs.llvmPackages.libclang

            # General utilities
            pkgs.pkg-config
            pkgs.openssl
          ]
          ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
            pkgs.udev # needed for serial port access on Linux
          ];

          # ── Environment ───────────────────────────────────────────────

          # bindgen needs to find libclang.so
          LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";

          # Point embuild at the Nix-fetched ESP-IDF; prevents it from
          # cloning its own copy at build time.
          IDF_PATH = "${espIdf}";
          IDF_PYTHON_ENV_PATH = "${espIdfPython}";
          IDF_PYTHON_CHECK_DONE = "1";

          ESP_IDF_TOOLS_INSTALL_DIR = "global"; # managed by the nix flake

          # Must match the tag we pinned above.
          ESP_IDF_VERSION = espIdfVersion;

          MCU = "esp32s2";

          shellHook = ''
            export IDF_PYTHON_ENV_PATH="${espIdfPython}"
            export GIT_CONFIG_COUNT=1
            export GIT_CONFIG_KEY_0="safe.directory"
            export GIT_CONFIG_VALUE_0="${espIdf}"
            export ESP_IDF_TOOLS_INSTALL_DIR="global"

            # Put the Xtensa toolchain bins ahead of everything else so
            # they shadow any system rustc / cargo.
            export PATH="${xtensaRust}/bin:${xtensaEspElf}/bin:$PATH"

            # rust-analyzer and IDEs look here for the Rust standard library source.
            export RUST_SRC_PATH="${xtensaRust}/lib/rustlib/src/rust/library"

            # The Xtensa toolchain installs as the "esp" toolchain name.
            # cargo +esp build ... uses this.
            export RUSTUP_TOOLCHAIN="esp"

            echo ""
            echo "  🦀  ESP32-S Rust WebSocket dev environment"
            echo "  ──────────────────────────────────────────"
            echo "  Rust  : $(rustc --version 2>/dev/null || echo 'ERROR — check PATH')"
            echo "  Cargo : $(cargo --version 2>/dev/null || echo 'ERROR — check PATH')"
            echo "  GCC   : $(xtensa-esp32s2-elf-gcc --version 2>/dev/null | head -1 || echo 'not found')"
            echo "  IDF   : $IDF_PATH"
            echo "  MCU   : $MCU"
            echo ""
            echo "  Quick start:"
            echo "    cargo generate esp-rs/esp-idf-template"
            echo "    cd <project>"
            echo "    cargo build --release"
            echo "    espflash flash --monitor"
            echo ""
            echo "  WebSocket crates to add to Cargo.toml:"
            echo "    esp-idf-svc        WiFi, TCP/IP, HTTP + WebSocket server"
            echo "    embedded-svc       trait definitions used by esp-idf-svc"
            echo ""
          '';
        };

        # ════════════════════════════════════════════════════════════════
        # INDIVIDUAL PACKAGES
        #   nix build .#xtensaRust
        #   nix build .#xtensaEspElf
        #   nix build .#ldproxy
        # ════════════════════════════════════════════════════════════════
        packages = {
          inherit xtensaRust xtensaEspElf ldproxy;
          default = xtensaRust;
        };
      }
    );
}
