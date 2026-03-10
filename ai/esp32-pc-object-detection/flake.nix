# ESP32-S2 Rust Development Flake
# ================================
# Targets: xtensa-esp32s2-espidf
# Provides: esp-idf, xtensa-esp-elf GCC, Xtensa Rust (esp-rs), espflash
#
# FIRST RUN — fill in the sha256 placeholders:
#   Each `sha256 = lib.fakeHash` will error and print the correct hash.
#   Substitute each one, then re-run until the shell opens cleanly.
#
# PROJECT SETUP (add these files to your cargo project):
#   .cargo/config.toml  — see bottom of this file
#   rust-toolchain.toml — see bottom of this file
#   build.rs            — standard esp-idf-sys build.rs
{
  description = "ESP32-S2 Rust/ESP-IDF development environment (x86_64-linux)";

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
    # Only expose the x86_64-linux shell; the pre-built Xtensa blobs are
    # Linux-only anyway.
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
        lib = pkgs.lib;

        # ── Version pins ────────────────────────────────────────────────────
        # ESP-IDF stable release
        idfVersion = "5.3.2";

        # GCC Xtensa cross-compiler matching IDF 5.3.x tools.json
        # Check: $IDF_PATH/tools/tools.json → "xtensa-esp-elf" → version
        xtensaGccVersion = "esp-13.2.0_20240530";

        # esp-rs Xtensa-patched Rust release
        # https://github.com/esp-rs/rust-build/releases
        xtensaRustVersion = "1.85.0.0";

        # ── ESP-IDF ─────────────────────────────────────────────────────────
        # `leaveDotGit = true` keeps the .git metadata so that:
        #   • cmake/version.cmake can run `git describe` without erroring
        #   • idf_component_manager can resolve the IDF tag
        # `fetchSubmodules = true` pulls the mandatory bundled components.
        espIdf = pkgs.fetchgit {
          url = "https://github.com/espressif/esp-idf.git";
          rev = "v${idfVersion}";
          sha256 = "sha256-lY0ZYaTKgKU4xGt8xIqxAQyZoh5IpasuiSL5UJIhs48="; # ← replace after first `nix develop`
          fetchSubmodules = true;
          leaveDotGit = true;
        };

        # ── Xtensa GCC cross-toolchain ──────────────────────────────────────
        # Pre-built binaries from Espressif's crosstool-NG releases.
        # Contains xtensa-esp-elf-{gcc,g++,ld,objcopy,…} for all Xtensa targets
        # including ESP32-S2.
        xtensaEspElf = pkgs.stdenv.mkDerivation {
          pname = "xtensa-esp-elf";
          version = xtensaGccVersion;

          src = pkgs.fetchurl {
            # url = "https://github.com/espressif/crosstool-NG/releases/download/${xtensaGccVersion}/xtensa-esp-elf-${xtensaGccVersion}-x86_64-linux-gnu.tar.xz";
            url = "https://github.com/espressif/crosstool-NG/releases/download/esp-13.2.0_20240530/xtensa-esp-elf-13.2.0_20240530-x86_64-linux-gnu.tar.xz";
            sha256 = "sha256-/O8D2H6sRMDb7iu+6YRD7S/PgnINzY6/4AZAgHsPB8I="; # ← replace
          };

          # autoPatchelfHook rewrites ELF interpreter paths for NixOS —
          # without this every binary in the tarball will segfault or "not found".
          nativeBuildInputs = [ pkgs.autoPatchelfHook ];
          buildInputs = with pkgs; [
            stdenv.cc.libc
            stdenv.cc.cc.lib
            ncurses
            zlib
          ];

          dontConfigure = true;
          dontBuild = true;

          installPhase = ''
            runHook preInstall
            mkdir -p "$out"
            # The tarball unpacks into a single top-level directory;
            # cp -r moves all contents into $out.
            cp -r . "$out/"
            runHook postInstall
          '';
        };

        # ── Xtensa Rust compiler ────────────────────────────────────────────
        # The esp-rs project maintains a fork of rustc patched for the Xtensa
        # ISA (ESP32 / ESP32-S2 / ESP32-S3).  Mainline Rust does NOT support
        # these targets; only RISC-V ESP32 variants (C3/C6/H2) use upstream Rust.
        #
        # Archive layout mirrors rustup-dist:
        #   rust-<ver>-x86_64-unknown-linux-gnu/
        #     install.sh
        #     rustc/bin/rustc  …
        #     cargo/bin/cargo  …
        xtensaRust = pkgs.stdenv.mkDerivation rec {
          pname = "xtensa-esp-rust";
          version = xtensaRustVersion;

          src = pkgs.fetchurl {
            url = "https://github.com/esp-rs/rust-build/releases/download/v${version}/rust-${version}-x86_64-unknown-linux-gnu.tar.xz";
            sha256 = "sha256-0TvX+Ruc1XkV75Q3/dbkOk8EOJUvfzAYKbqP43Xe5xc="; # ← replace
          };

          nativeBuildInputs = with pkgs; [
            autoPatchelfHook
            makeWrapper
          ];
          buildInputs = with pkgs; [
            stdenv.cc.libc
            stdenv.cc.cc.lib
            zlib
            openssl
            libxml2
            ncurses
            curl
          ];

          dontConfigure = true;
          dontBuild = true;

          installPhase = ''
            runHook preInstall
            patchShebangs install.sh
            # The upstream install.sh honours --prefix and --disable-ldconfig.
            bash install.sh --prefix="$out" --disable-ldconfig

            mkdir -p "$out/lib/rustlib/src/rust"
            ln -s "${xtensaRustSrc}/lib/rustlib/src/rust/library" "$out/lib/rustlib/src/rust/library"

            runHook postInstall
          '';

          # Make sure rustc reports a sensible sysroot without RUSTUP_HOME.
          postFixup = ''
            wrapProgram "$out/bin/rustc" \
              --set-default RUSTUP_TOOLCHAIN esp
          '';
        };

        # rust-src component — required for `build-std` which rebuilds core/std
        # for the bare-metal xtensa-esp32s2-espidf target.
        xtensaRustSrc = pkgs.stdenv.mkDerivation rec {
          pname = "xtensa-esp-rust-src";
          version = xtensaRustVersion;

          src = pkgs.fetchurl {
            url = "https://github.com/esp-rs/rust-build/releases/download/v${version}/rust-src-${version}.tar.xz";
            sha256 = "sha256-hnFVoM7k+9ifQNkBKnfQmJVERjI01lEld9meFsGHCHI="; # ← replace
          };

          dontConfigure = true;
          dontBuild = true;

          installPhase = ''
            runHook preInstall
            patchShebangs install.sh
            bash install.sh --prefix="$out" --disable-ldconfig
            runHook postInstall
          '';
        };

        # ── Python environment for ESP-IDF ──────────────────────────────────
        # IDF requires a Python environment with various packages.  Normally
        # `idf_tools.py install` creates a venv in ~/.espressif/python_env.
        # By pointing IDF_PYTHON_ENV_PATH at this Nix env we satisfy embuild's
        # parser and skip the venv-creation step entirely.
        #
        # If a package below is missing from nixpkgs 24.11, add it with
        # `buildPythonPackage` or drop it — IDF will tolerate extras.
        espPython = pkgs.python3.withPackages (
          ps: with ps; [
            pip
            setuptools
            wheel
            pyserial
            click
            cryptography
            future
            pyparsing
            pyelftools
            construct
            requests
            urllib3
            chardet
            certifi
            idna
            packaging
            pyyaml
            kconfiglib
            # idf-component-manager is sometimes packaged, try it:
            # idf-component-manager
          ]
        );

      in
      {
        # ── Exposed packages (optional, for `nix build .#xtensaRust` etc.) ──
        packages = {
          inherit
            espIdf
            xtensaEspElf
            xtensaRust
            xtensaRustSrc
            espPython
            ;
        };

        # ── Development shell ───────────────────────────────────────────────
        devShells.default = pkgs.mkShell {
          name = "esp32s2-rust";

          buildInputs = with pkgs; [
            # ── Native build tools ──────────────────────────────────────
            cmake
            ninja
            git
            wget
            curl
            flex
            bison
            gperf
            ccache
            dfu-util
            libusb1
            pkg-config
            openssl
            openssl.dev

            # ── Python ──────────────────────────────────────────────────
            espPython

            # ── Xtensa toolchains ────────────────────────────────────────
            xtensaEspElf
            xtensaRust
            xtensaRustSrc

            # ── Flash / monitor ─────────────────────────────────────────
            # espflash provides the `espflash` CLI (flash + monitor).
            # cargo-espflash wraps it as a Cargo subcommand (`cargo espflash`).
            espflash
          ];

          shellHook = ''
            # ── IDF location ────────────────────────────────────────────────
            export IDF_PATH="${espIdf}"

            # Where idf_tools.py *would* install native tools.  We never
            # actually run that; it just needs to be a writable directory.
            export IDF_TOOLS_PATH="''${IDF_TOOLS_PATH:-$HOME/.espressif}"

            # ── Fix: IDF_PYTHON_ENV_PATH missing from idf_tools.py export ───
            # embuild parses `idf_tools.py export --format key-value` and
            # errors if IDF_PYTHON_ENV_PATH is absent.  Pointing it at our
            # Nix Python env short-circuits the whole venv machinery.
            export IDF_PYTHON_ENV_PATH="${espPython}"

            # ── Fix: bypass idf_tools.py entirely ───────────────────────────
            # With ESP_IDF_TOOLS_INSTALL_DIR=fromenv, embuild skips calling
            # idf_tools.py and uses PATH-resident tools instead.
            # This is the primary guard against the IDF_PYTHON_ENV_PATH error.
            export ESP_IDF_TOOLS_INSTALL_DIR="fromenv"

            # ── Toolchain binaries on PATH ───────────────────────────────────
            export PATH="${xtensaEspElf}/bin:${xtensaRust}/bin:$PATH"

            # ── Rust / Cargo configuration ───────────────────────────────────
            # Tell Cargo which rustc/cargo to use (avoids rustup version games).
            export RUSTC="${xtensaRust}/bin/rustc"
            export CARGO="${xtensaRust}/bin/cargo"

            # Needed by build-std so rustc can find the standard library source.
            export RUST_SRC_PATH="${xtensaRustSrc}/lib/rustlib/src/rust"

            # Default build target for this shell.
            export CARGO_BUILD_TARGET="xtensa-esp32s2-espidf"

            # rustup_toolchain name — harmless if rustup is not installed.
            export RUSTUP_TOOLCHAIN="esp"

            # ── OpenSSL / pkg-config (for -sys crates) ───────────────────────
            export PKG_CONFIG_PATH="${pkgs.openssl.dev}/lib/pkgconfig''${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
            export OPENSSL_DIR="${pkgs.openssl.dev}"
            export OPENSSL_LIB_DIR="${pkgs.openssl.out}/lib"
            export OPENSSL_INCLUDE_DIR="${pkgs.openssl.dev}/include"

            # ── ESP-IDF component manager ────────────────────────────────────
            export IDF_COMPONENT_MANAGER_SKIP_UPDATE_CHECK="1"

            # ── Scaffold .cargo/config.toml if absent ────────────────────────
            # if [ ! -f .cargo/config.toml ]; then
            #   mkdir -p .cargo
            #   cat > .cargo/config.toml << 'EOF'
            #   [build]
            #   target = "xt# ensa-esp32s2-espidf"

            #   [target.xten# sa-esp32s2-espidf]
            #   linker = "ld# proxy"
            #   # Pass -Wl,-# Map=output.map to the linker for debugging if needed.
            #   # runner = "# espflash flash --monitor"   # uncomment to auto-flash on `cargo run`

            #   [unstable]
            #   build-std = # ["std", "panic_abort"]
            #   EOF
            #   echo "[flake] created .cargo/config.toml"
            # fi

            # # ── Scaffold rust-toolchain.toml if absent ───────────────────────
            # if [ ! -f rust-toolchain.toml ]; then
            #   cat > rust-toolchain.toml << 'EOF'
            #   [toolchain]
            #   channel = "e# sp"
            #   EOF
            #   echo "[flake] created rust-toolchain.toml"
            # fi

            # echo ""
            # echo "╔══════════════════════════════════════════════════════╗"
            # echo "║         ESP32-S2 Rust development shell ready        ║"
            # echo "╠══════════════════════════════════════════════════════╣"
            # printf "║  IDF_PATH  : %-38s║\n" "$(echo ${espIdf} | cut -c1-38)"
            # printf "║  rustc     : %-38s║\n" "$(rustc --version 2>/dev/null | cut -c1-38 || echo 'not found')"
            # printf "║  espflash  : %-38s║\n" "$(espflash --version 2>/dev/null | head -1 | cut -c1-38 || echo 'not found')"
            # echo "╚══════════════════════════════════════════════════════╝"
            # echo ""
            # echo "  Build  : cargo build"
            # echo "  Flash  : espflash flash --monitor target/xtensa-esp32s2-espidf/debug/<bin>"
            # echo ""
          '';
        };
      }
    );
}

# ═══════════════════════════════════════════════════════════════════════════════
# PROJECT FILE TEMPLATES
# ═══════════════════════════════════════════════════════════════════════════════
#
# Cargo.toml (relevant section):
# ───────────────────────────────
# [dependencies]
# esp-idf-svc = { version = "0.49", features = ["experimental"] }
# esp-idf-sys = { version = "0.35", features = ["binstart"] }
#
# [build-dependencies]
# embuild = "0.32"
#
# [profile.release]
# opt-level = "s"
#
# ───────────────────────────────
# build.rs:
# ───────────────────────────────
# fn main() {
#     embuild::espidf::sysenv::output();
# }
#
# ───────────────────────────────
# sdkconfig.defaults:
# ───────────────────────────────
# CONFIG_ESP_SYSTEM_EVENT_TASK_STACK_SIZE=4096
# CONFIG_PTHREAD_TASK_STACK_SIZE_DEFAULT=8192
#
# ═══════════════════════════════════════════════════════════════════════════════
# HASH REPLACEMENT WORKFLOW
# ═══════════════════════════════════════════════════════════════════════════════
#
# 1. Run `nix develop` — it will fail on the first `lib.fakeHash` and print:
#      error: hash mismatch in fixed-output derivation
#        specified: sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
#           got:    sha256-<REAL_HASH>
#
# 2. Replace that `lib.fakeHash` with the printed sha256-… string.
# 3. Repeat until all hashes are filled and the shell opens.
#
# For espIdf (fetchgit with fetchSubmodules) this can take several minutes
# the first time — be patient.
