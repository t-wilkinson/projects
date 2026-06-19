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
  description = "ESP32 Rust/Xtensa/ESP-IDF development environment";

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
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
        isLinux = pkgs.stdenv.isLinux;
        # isDarwin = pkgs.stdenv.isDarwin;
        versions = import ./nix/versions.nix;

        # ESP-IDF packages
        espIdf = pkgs.callPackage ./nix/esp-idf.nix { inherit versions; };
        toolchain = pkgs.callPackage ./nix/toolchain.nix { inherit versions; };
        tools = pkgs.callPackage ./nix/tools.nix { };
        espTools = { } // espIdf // toolchain // tools;
      in
      {
        packages = { } // espTools;

        devShells.default =
          if isLinux then
            pkgs.mkShell {
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

              shellHook = pkgs.callPackage ./nix/devshell.nix { inherit versions espTools; };
            }
          else
            pkgs.mkShell {
              name = "esp32-ml";

              buildInputs = with pkgs; [
                tools.basePython
                git
                curl
              ];

              shellHook = ''
                # ── Python venv for extra pip deps ─────────────
                _ML_VENV="''${ML_VENV_PATH:-$PWD/.venv}"

                if [ ! -f "$_ML_VENV/pyvenv.cfg" ]; then
                  echo ""
                  echo "  [flake] Creating Python venv for ML/Jupyter..."
                  python3 -m venv "$_ML_VENV"
                  source "$_ML_VENV/bin/activate"
                  pip install --upgrade pip setuptools -q
                  if [ -f requirements.txt ]; then
                    pip install -r requirements.txt 2>&1 | grep -v 'already satisfied'
                  fi
                  echo "  [flake] venv ready."
                else
                  source "$_ML_VENV/bin/activate"
                fi

                # Register venv as a Jupyter kernel so we don't use nix's python
                python -m ipykernel install --user --name esp32-ml --display-name "ESP32 ML" --overwrite

                echo ""
                echo "  ESP32 ML shell (macOS — Jupyter only)"
                echo "  ────────────────────────────────────────"
                echo "  Python : $(python --version 2>/dev/null)"
                echo "  Jupyter: jupyter notebook (uses 'ESP32 ML' kernel)"
                echo ""
              '';
            };
      }
    );
}
