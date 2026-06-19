{
  lib,
  pkgs,
  versions,
  espTools,
}:
''
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
  export LD_LIBRARY_PATH="/run/opengl-driver/lib:/run/opengl-driver-32/lib:${
    lib.makeLibraryPath (
      with pkgs;
      [
        zlib
        stdenv.cc.cc.lib
        libxml2
        ncurses

        # OpenCV dependencies
        xorg.libxcb
        xorg.libX11
        xorg.libXext
        xorg.libXrender
        libGL
        glib
      ]
    )
  }''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
  export BINDGEN_EXTRA_CLANG_ARGS="-fsigned-char"
  echo $LD_LIBRARY_PATH

  # Python venv
  # python -m venv .venv
  # source .venv/bin/activate
  pip install -r requirements.txt 2>&1 | grep -v 'already satisfied'
  # Register venv as a Jupyter kernel so we don't use nix's python
  python -m ipykernel install --user --name esp32-ml --display-name "ESP32 ML"

  # ── Toolchain PATH ──────────────────────────────────────
  # IMPORTANT keep this after activating python env
  export PATH="${espTools.xtensaRust}/bin:${espTools.xtensaEspElf}/bin:${espTools.ldproxy}/bin:${espTools.espIdf}/tools:${espTools.espIdf}:$PATH"

  echo ""
  echo "  ESP32 Rust development shell"
  echo "  ────────────────────────────────────────"
  echo "  Rust  : $(rustc --version 2>/dev/null || echo 'ERROR')"
  echo "  Cargo : $(cargo --version 2>/dev/null || echo 'ERROR')"
  echo "  GCC   : $(xtensa-esp32-elf-gcc --version 2>/dev/null | head -1 || echo 'not found')"
  echo "  IDF   : $IDF_PATH  (v${versions.idf})"
  echo "  Python: $(python --version 2>/dev/null) (venv)"
  echo "  MCU   : $MCU"
  echo ""
  echo "  Jupyter : jupyter notebook (uses 'ESP32 ML' kernel)"
  echo "  Build : cargo build --release"
  echo "  Flash : cargo run --release"
  echo "       or espflash flash --monitor target/xtensa-esp32-espidf/debug/<bin>"
  echo ""
''
