# nix/esp-idf.nix - ESP-IDF SOURCE + PATCHES
{ pkgs, versions }:
rec {
  espIdfSrc = pkgs.fetchgit {
    url = "https://github.com/espressif/esp-idf.git";
    rev = "v${versions.idf}";
    hash = "sha256-lY0ZYaTKgKU4xGt8xIqxAQyZoh5IpasuiSL5UJIhs48=";
    fetchSubmodules = true;
    leaveDotGit = true;
  };

  # Wrapper: fix .git, write version.txt, and — critically —
  # patch the cmake build system so it never calls git_describe.
  # GetGitRevisionDescription.cmake fails in Nix because
  # fetchgit strips refs that grabRef.cmake needs.
  espIdf =
    pkgs.runCommand "esp-idf-v${versions.idf}"
      {
        nativeBuildInputs = [
          pkgs.git
          pkgs.python3
        ];
      }
      ''
                cp -a ${espIdfSrc} $out
                chmod -R u+w $out

                cd $out

                # ── Write version.txt ──────────────────────────────────────
                echo "v${versions.idf}" > version.txt

                # ── Fix .git so ad-hoc git calls still work ────────────────
                git config user.email "nix@build"
                git config user.name  "nix"
                git tag -f "v${versions.idf}" HEAD 2>/dev/null || true

                # ── Patch cmake to skip git_describe entirely ──────────────
                # Replace __build_get_idf_git_revision in build.cmake so it
                # reads version.txt instead of calling git_describe (which
                # triggers GetGitRevisionDescription → grabRef → crash).
                # Also patch __build_check_python to be a no-op — the Nix
                # venv may have slightly different versions than IDF's strict
                # constraints and that's fine.
                cat > /tmp/patch_build_cmake.py << 'PYEOF'
        import re, sys

        path = sys.argv[1]
        version = sys.argv[2]

        with open(path, "r") as f:
            content = f.read()

        # --- Patch 1: __build_get_idf_git_revision ---
        # The original calls git_describe which is a CMake MACRO (no scope).
        # PARENT_SCOPE only propagates one level, but this is called from
        # nested functions.  Use a macro so IDF_VER_GIT is set directly in
        # the caller's scope, matching the original git_describe behavior.
        git_replacement = (
            "macro(__build_get_idf_git_revision)\n"
            "    set(IDF_VER_GIT \"v" + version + "\")\n"
            "    add_compile_definitions(IDF_VER=\"v" + version + "\")\n"
            "endmacro()"
        )

        pattern = r'function\(__build_get_idf_git_revision\).*?endfunction\(\)'
        content, c1 = re.subn(pattern, git_replacement, content, count=1, flags=re.DOTALL)
        print("Patched __build_get_idf_git_revision: " + str(c1))

        # --- Patch 2: __build_check_python ---
        # Make the python dependency check a no-op so cmake doesn't fail
        # on version mismatches like cryptography 46 vs <43.
        py_replacement = (
            "function(__build_check_python)\n"
            "    # Patched by nix flake: skip python dependency check\n"
            "endfunction()"
        )

        pattern2 = r'function\(__build_check_python\).*?endfunction\(\)'
        content, c2 = re.subn(pattern2, py_replacement, content, count=1, flags=re.DOTALL)
        print("Patched __build_check_python: " + str(c2))

        with open(path, "w") as f:
            f.write(content)
        PYEOF

                python3 /tmp/patch_build_cmake.py \
                  "$out/tools/cmake/build.cmake" \
                  "${versions.idf}"

                # ── Patch GetGitRevisionDescription.cmake ────────────
                # The bootloader subproject (and anything using project.cmake)
                # calls git_describe via this module.  grabRef.cmake crashes
                # when .git refs are incomplete (Nix fetchgit strips them).
                # Overwrite the entire file with stubs that return fixed values.
                cat > "$out/tools/cmake/third_party/GetGitRevisionDescription.cmake" << 'CMEOF'
        # Patched by nix flake: all git queries return fixed values.
        # This avoids grabRef.cmake crashes in the Nix store.

        function(get_git_head_revision _refspecvar _hashvar)
            set(''${_refspecvar} "refs/tags/v${versions.idf}" PARENT_SCOPE)
            set(''${_hashvar} "0000000000000000000000000000000000000000" PARENT_SCOPE)
        endfunction()

        function(git_describe _var)
            set(''${_var} "v${versions.idf}" PARENT_SCOPE)
        endfunction()

        function(git_describe_working_tree _var)
            set(''${_var} "v${versions.idf}" PARENT_SCOPE)
        endfunction()

        function(git_get_exact_tag _var)
            set(''${_var} "v${versions.idf}" PARENT_SCOPE)
        endfunction()

        function(git_local_changes _var)
            set(''${_var} "CLEAN" PARENT_SCOPE)
        endfunction()
        CMEOF
      '';

}
