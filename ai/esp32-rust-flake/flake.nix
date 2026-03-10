# ESP32-S2 Rust + Xtensa + ESP-IDF — Nix Flake
# ==============================================
# Target  : xtensa-esp32-espidf
# Provides: esp-idf (patched cmake), xtensa-esp-elf GCC, Xtensa Rust (esp-rs),
#           rust-src, ldproxy, espflash, Python venv
#
# KNOWN ISSUES THIS FLAKE SOLVES:
#   1. ESP-IDF .git / git describe — cmake patched to read version.txt directly,
#      no .git needed at build time.
#   2. Read-only Nix store — ESP_IDF_TOOLS_INSTALL_DIR=fromenv tells embuild
#      to use PATH-resident tools instead of writing to IDF_PATH.
#   3. IDF Python deps — a real venv is created in ~/.espressif/ on first
#      `nix develop`, pip-installing all IDF requirements (esptool, etc.).
#   4. Version mismatches — all versions are pinned and cross-referenced.
#
# USAGE:
#   nix develop          # enter the dev shell (first run creates Python venv)
#   cargo build          # build your ESP32 project
#   espflash flash --monitor target/xtensa-esp32-espidf/debug/<bin>
{
  description = "ESP32-S2 Rust/Xtensa/ESP-IDF development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
        lib = pkgs.lib;
        versions = import ./nix/versions.nix;
        espIdf = pkgs.callPackage ./nix/esp-idf.nix { inherit versions; };
        toolchain = pkgs.callPackage ./nix/toolchain.nix { inherit versions; };
        tools = pkgs.callPackage ./nix/tools.nix { inherit versions; };
        espTools = { } // espIdf // toolchain // tools;
      in
      {
        packages = { } // espTools;

        devShells.default = pkgs.mkShell {
          name = "esp32-rust";

          buildInputs = with pkgs; [
            cmake
            ninja
            git
            wget
            curl
            websocat
            flex
            bison
            gperf
            ccache
            dfu-util
            libusb1
            pkg-config
            openssl
            openssl.dev
            clang
            llvmPackages.libclang
            espTools.basePython
            espTools.xtensaEspElf
            espTools.xtensaRust
            espTools.xtensaRustSrc
            espTools.ldproxy
            espflash
          ];

          LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";

          shellHook = ''
            # ── ESP-IDF ─────────────────────────────────────────────
            export IDF_PATH="${espTools.espIdf}"
            export ESP_IDF_VERSION="v${versions.idf}"
            export ESP_IDF_TOOLS_INSTALL_DIR="fromenv"

            export IDF_TOOLS_PATH="''${IDF_TOOLS_PATH:-$HOME/.espressif}"
            mkdir -p "$IDF_TOOLS_PATH"

            # ── Python venv ─────────────────────────────────────────
            # Create a pip-based venv on first run that satisfies ALL
            # of IDF's Python requirements.  Subsequent runs just
            # activate the existing venv.
            _IDF_VENV="$IDF_TOOLS_PATH/python_env/idf${versions.idf}_nix"

            if [ ! -f "$_IDF_VENV/pyvenv.cfg" ]; then
              echo ""
              echo "  [flake] Creating Python venv for ESP-IDF v${versions.idf}..."
              echo "  [flake] This only happens once.  It may take a minute."
              echo ""
              python3 -m venv "$_IDF_VENV"
              source "$_IDF_VENV/bin/activate"
              pip install --upgrade pip setuptools -q

              # Download Espressif's version-constraint file so pip
              # installs the exact versions IDF expects (e.g. esptool~=4.8,
              # cryptography<43, etc.)
              _CONSTRAINTS="$IDF_TOOLS_PATH/espidf.constraints.v5.3.txt"
              if [ ! -f "$_CONSTRAINTS" ]; then
                curl -sSL -o "$_CONSTRAINTS" \
                  "https://dl.espressif.com/dl/esp-idf/espidf.constraints.v5.3.txt" \
                  || echo "  [flake] WARNING: could not download constraints file"
              fi

              _C_FLAG=""
              if [ -f "$_CONSTRAINTS" ]; then
                _C_FLAG="-c $_CONSTRAINTS"
              fi

              # Install from IDF's own requirement files with constraints.
              if [ -f "$IDF_PATH/tools/requirements/requirements.core.txt" ]; then
                pip install $_C_FLAG \
                  -r "$IDF_PATH/tools/requirements/requirements.core.txt" \
                  -q
              fi

              # Top-level catch-all (may overlap, pip handles that)
              if [ -f "$IDF_PATH/tools/requirements.txt" ]; then
                pip install $_C_FLAG \
                  -r "$IDF_PATH/tools/requirements.txt" \
                  -q 2>/dev/null || true
              fi

              echo "  [flake] Python venv ready."
            else
              source "$_IDF_VENV/bin/activate"
            fi

            export IDF_PYTHON_ENV_PATH="$_IDF_VENV"
            export IDF_PYTHON_CHECK_DONE="1"

            # ── Git safe-directory ──────────────────────────────────
            export GIT_CONFIG_COUNT=1
            export GIT_CONFIG_KEY_0="safe.directory"
            export GIT_CONFIG_VALUE_0="*"

            # ── Toolchain PATH ──────────────────────────────────────
            export PATH="${espTools.xtensaRust}/bin:${espTools.xtensaEspElf}/bin:${espTools.ldproxy}/bin:${espIdf}/tools:$PATH"

            # ── Rust / Cargo ────────────────────────────────────────
            export RUSTC="${espTools.xtensaRust}/bin/rustc"
            export CARGO="${espTools.xtensaRust}/bin/cargo"
            export RUSTUP_TOOLCHAIN="esp"
            export RUST_SRC_PATH="${espTools.xtensaRustSrc}/lib/rustlib/src/rust/library"
            export CARGO_BUILD_TARGET="xtensa-esp32-espidf"
            export MCU="esp32"

            # ── OpenSSL / pkg-config ────────────────────────────────
            export PKG_CONFIG_PATH="${pkgs.openssl.dev}/lib/pkgconfig''${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
            export OPENSSL_DIR="${pkgs.openssl.dev}"
            export OPENSSL_LIB_DIR="${pkgs.openssl.out}/lib"
            export OPENSSL_INCLUDE_DIR="${pkgs.openssl.dev}/include"

            # ── Misc ────────────────────────────────────────────────
            export IDF_COMPONENT_MANAGER_SKIP_UPDATE_CHECK="1"

            # ── libclang for bindgen ─────────────────────────────
            # The esp-rs toolchain's build script overrides LIBCLANG_PATH
            # to use its bundled esp-clang from ~/.rustup/toolchains/esp/.
            # We can't easily prevent that, so instead ensure all shared
            # libraries that libclang.so needs are on LD_LIBRARY_PATH.
            export LIBCLANG_PATH="${pkgs.llvmPackages.libclang.lib}/lib"
            export LD_LIBRARY_PATH="${
              lib.makeLibraryPath [
                pkgs.zlib
                pkgs.stdenv.cc.cc.lib
                pkgs.libxml2
                pkgs.ncurses
              ]
            }''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
            export BINDGEN_EXTRA_CLANG_ARGS="-fsigned-char"

            echo ""
            echo "  ESP32-S2 Rust development shell"
            echo "  ────────────────────────────────────────"
            echo "  Rust  : $(rustc --version 2>/dev/null || echo 'ERROR')"
            echo "  Cargo : $(cargo --version 2>/dev/null || echo 'ERROR')"
            echo "  GCC   : $(xtensa-esp32-elf-gcc --version 2>/dev/null | head -1 || echo 'not found')"
            echo "  IDF   : $IDF_PATH  (v${versions.idf})"
            echo "  Python: $(python --version 2>/dev/null) (venv)"
            echo "  MCU   : $MCU"
            echo ""
            echo "  Build : cargo build"
            echo "  Flash : espflash flash --monitor target/xtensa-esp32-espidf/debug/<bin>"
            echo ""
          '';
        };
      }
    );
}
