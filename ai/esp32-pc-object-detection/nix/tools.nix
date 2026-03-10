# nix/tools.nix - misc tools
{ pkgs }:
{
  # ════════════════════════════════════════════════════════════════
  # LDPROXY  (required by esp-idf-sys / embuild)
  # ════════════════════════════════════════════════════════════════
  ldproxy = pkgs.rustPlatform.buildRustPackage rec {
    pname = "ldproxy";
    version = "0.3.3";

    src = pkgs.fetchCrate {
      inherit pname version;
      hash = "sha256-XLfa40eMkeUL544gDqZYbly2E5Mrogn7v24D8u/wjkg=";
    };
    cargoHash = "sha256-h7WOslRfu7cQ/af/b6C8gN2QrEt2SLxNnGeEv6bKj3E=";
    meta.description = "Linker proxy for esp-idf-sys / embuild";
  };

  # ════════════════════════════════════════════════════════════════
  # BASE PYTHON  (just enough to create a venv + pip install)
  # ════════════════════════════════════════════════════════════════
  # Many IDF Python deps (esptool, idf-component-manager, etc.)
  # are pip-only and not in nixpkgs.  The shellHook creates a
  # venv and pip-installs from IDF's requirements files.
  basePython = pkgs.python3.withPackages (
    ps: with ps; [
      pip
      setuptools
      wheel
      virtualenv
    ]
  );
}
