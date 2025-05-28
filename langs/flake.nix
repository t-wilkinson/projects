{
  description = "Ready-made templates for easily creating flake-driven environments";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forEachSupportedSystem =
        f:
        nixpkgs.lib.genAttrs supportedSystems (
          system:
          f {
            inherit system;
            pkgs = import nixpkgs { inherit system; };
          }
        );

      mkApp = scriptName: system: text: {
        type = "app";
        program = "${
          (nixpkgs.legacyPackages.${system}.writeScriptBin scriptName ''
            #!/usr/bin/env bash
            set -euo pipefail

            RED="\033[0;31m"
            GREEN="\033[0;32m"
            YELLOW="\033[1;33m"
            BLUE="\033[0;34m"
            BOLD="\033[1m"
            RESET="\033[0m"

            # Logging function
            log() {
              local level="$1"
              local message="$2"
              case "$level" in
                info) color="$BLUE";;
                success) color="$GREEN";;
                warn) color="$YELLOW";;
                error) color="$RED";;
                *) color="$RESET";;
              esac
              if [ "$level" == info ]; then
                echo -e "''${BOLD}''${color}$message''${RESET}"
              else
                echo -e "''${BOLD}''${color}[$level]''${RESET} $message"
              fi
            }

            ${text}
          '')
        }/bin/${scriptName}";
      };

      forEachDir = execCmd: ''
        for dir in c haskell java nix node php python r rust-toolchain tex; do
          (
            cd "$dir" || continue
            if [ ! -f flake.nix ]; then
              log warn "Skipping $dir (no flake.nix)"
            elif [ ! -f flake.lock ]; then
              log warn "Skipping $dir (no flake.lock)"
            else
              log info "Running command in $dir"
              ${execCmd}
            fi
          )
        done
      '';
    in
    {
      apps = forEachSupportedSystem (
        { system, pkgs }:
        {
          update = mkApp "update" system (forEachDir ''
            nix flake update
          '');
          build = mkApp "build" system (forEachDir ''
            nix build ".#devShells.${system}.default"
          '');
        }
      );
    };
}
