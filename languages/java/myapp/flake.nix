{
  description = "Java learning project";

  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11"; };

  outputs = { self, nixpkgs, ... }:
    let
      supportedSystems =
        [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = attrs:
        nixpkgs.lib.mapAttrs (_: f:
          nixpkgs.lib.genAttrs supportedSystems
          (system: f { pkgs = import nixpkgs { inherit system; }; })) attrs;
    in forEachSupportedSystem {
      devShells = { pkgs }: {
        default = pkgs.mkShell {
          # buildInputs = with pkgs; [ maven openjdk ];
          shellHook = ''
            alias run_app='mvn exec:java -Dexec.mainClass="com.treywilkinson.App"'
          '';
        };
      };

      packages = { pkgs }: {
        run_app = pkgs.writeShellApplication {
          name = "run_app";
          # runtimeInputs = with pkgs; [ maven jdk ];
          text = ''
            mvn exec:java -Dexec.mainClass="com.treywilkinson.App" 
          '';
        };
      };
    };
}
