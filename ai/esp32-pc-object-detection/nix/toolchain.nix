# nix/toolchain.nix - Toolchain components: xtensaEspElf, xtensaRust, xtensaRustSrc, espClang
{ pkgs, versions }:
rec {
  # ════════════════════════════════════════════════════════════════
  # XTENSA GCC CROSS-COMPILER
  # ════════════════════════════════════════════════════════════════
  xtensaEspElf = pkgs.stdenv.mkDerivation {
    pname = "xtensa-esp-elf";
    version = versions.xtensaGccVer;

    src = pkgs.fetchurl {
      url = "https://github.com/espressif/crosstool-NG/releases/download/${versions.xtensaGccTag}/xtensa-esp-elf-${versions.xtensaGccVer}-x86_64-linux-gnu.tar.xz";
      hash = "sha256-/O8D2H6sRMDb7iu+6YRD7S/PgnINzY6/4AZAgHsPB8I=";
    };

    nativeBuildInputs = [ pkgs.autoPatchelfHook ];
    buildInputs = with pkgs; [
      stdenv.cc.libc
      stdenv.cc.cc.lib
      ncurses5
      zlib
      python3
    ];

    dontConfigure = true;
    dontBuild = true;
    dontStrip = true;

    installPhase = ''
      runHook preInstall
      mkdir -p "$out"
      cp -a . "$out/"
      runHook postInstall
    '';
  };

  # ════════════════════════════════════════════════════════════════
  # XTENSA RUST COMPILER  (esp-rs fork)
  # ════════════════════════════════════════════════════════════════
  xtensaRustSrc = pkgs.stdenv.mkDerivation rec {
    pname = "xtensa-rust-src";
    version = versions.xtensaRust;

    src = pkgs.fetchurl {
      url = "https://github.com/esp-rs/rust-build/releases/download/v${version}/rust-src-${version}.tar.xz";
      hash = "sha256-hnFVoM7k+9ifQNkBKnfQmJVERjI01lEld9meFsGHCHI=";
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

  xtensaRust = pkgs.stdenv.mkDerivation rec {
    pname = "xtensa-rust";
    version = versions.xtensaRust;

    src = pkgs.fetchurl {
      url = "https://github.com/esp-rs/rust-build/releases/download/v${version}/rust-${version}-x86_64-unknown-linux-gnu.tar.xz";
      hash = "sha256-0TvX+Ruc1XkV75Q3/dbkOk8EOJUvfzAYKbqP43Xe5xc=";
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
    dontStrip = true;

    installPhase = ''
      runHook preInstall
      patchShebangs install.sh
      bash install.sh --prefix="$out" --disable-ldconfig

      mkdir -p "$out/lib/rustlib/src/rust"
      ln -sf "${xtensaRustSrc}/lib/rustlib/src/rust/library" \
             "$out/lib/rustlib/src/rust/library"
      runHook postInstall
    '';

    postFixup = ''
      wrapProgram "$out/bin/rustc" \
        --set-default RUSTUP_TOOLCHAIN esp
    '';
  };

  # ════════════════════════════════════════════════════════════════
  # LIBCLANG FOR BINDGEN  (Espressif LLVM fork)
  # ════════════════════════════════════════════════════════════════
  # esp-idf-sys runs bindgen with `-target xtensa`. Upstream clang
  # rejects that triple ("unknown target triple 'xtensa'"), so bindgen
  # must load Espressif's libclang. This is the same archive espup
  # installs for the 1.85 toolchain.
  espClang = pkgs.stdenv.mkDerivation {
    pname = "esp-clang-libs";
    version = versions.espClang;

    src = pkgs.fetchurl {
      url = "https://github.com/espressif/llvm-project/releases/download/esp-${versions.espClang}/libs-clang-esp-${versions.espClang}-x86_64-linux-gnu.tar.xz";
      hash = "sha256-2JxLg9co9f8/Lp7/OGI+XG5mI70vHuOhPWrhEL++4Qs=";
    };

    nativeBuildInputs = [ pkgs.autoPatchelfHook ];
    buildInputs = with pkgs; [
      stdenv.cc.cc.lib
      zlib
      libxml2
    ];

    dontConfigure = true;
    dontBuild = true;
    dontStrip = true;

    installPhase = ''
      runHook preInstall
      mkdir -p "$out"
      cp -a . "$out/"
      runHook postInstall
    '';
  };
}
