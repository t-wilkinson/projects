{
  isLinux,
  pkgs,
  tools,
  prjGitPath,
  commonLibs,
  ...
}:
pkgs.mkShell {
  name = "esp32-ml";
  buildInputs =
    with pkgs;
    [
      tools.basePython
      git
      curl
      nodejs
    ]
    ++ pkgs.lib.optionals isLinux commonLibs;

  shellHook = ''
    ${
      if isLinux then
        ''
          export LD_LIBRARY_PATH="/run/opengl-driver/lib:/run/opengl-driver-32/lib:${pkgs.lib.makeLibraryPath (commonLibs)}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
        ''
      else
        ""
    }
              PRJ_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || pwd)
              _ML_VENV="''${ML_VENV_PATH:-$PRJ_ROOT/${prjGitPath}/models/.venv}"

              if [ ! -f "$_ML_VENV/pyvenv.cfg" ]; then
                echo "  [flake] Creating Python venv for ML/Jupyter..."
                python3 -m venv "$_ML_VENV"
                source "$_ML_VENV/bin/activate"
                pip install --upgrade pip setuptools -q

                if [ -f "PRJ_ROOT/models/requirements.txt" ]; then
                  pip install -r "PRJ_ROOT/models/requirements.txt" 2>&1 | grep -v 'already satisfied'
                fi
                echo "  [flake] venv ready."
              else
                source "$_ML_VENV/bin/activate"
              fi

              python -m ipykernel install --user --name esp32-ml --display-name "ESP32 ML" --overwrite

              echo "  ESP32 ML shell"
              echo "  Jupyter: jupyter notebook (uses 'ESP32 ML' kernel)"
  '';
}
