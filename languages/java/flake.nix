{
  description = "A Nix-flake-based Java development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs =
    { self, nixpkgs }:
    # let
    #   supportedSystems =
    #     [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    #   forEachSupportedSystem = attrs:
    #     nixpkgs.lib.mapAttrs (_: f:
    #       nixpkgs.lib.genAttrs supportedSystems
    #       (system: f { pkgs = import nixpkgs { inherit system; }; })) attrs;
    # in forEachSupportedSystem {
    #   devShells = { pkgs }: {
    #     default = pkgs.mkShell {
    #       # buildInputs = with pkgs; [ maven openjdk ];
    #       shellHook = ''
    #         alias run_app='mvn exec:java -Dexec.mainClass="com.treywilkinson.App"'
    #       '';
    #     };
    #   };

    #   packages = { pkgs }: {
    #     run_app = pkgs.writeShellApplication {
    #       name = "run_app";
    #       # runtimeInputs = with pkgs; [ maven jdk ];
    #       text = ''
    #         mvn exec:java -Dexec.mainClass="com.treywilkinson.App"
    #       '';
    #     };
    #   };
    # };

    let
      javaVersion = 23; # Change this value to update the whole stack

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
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ self.overlays.default ];
            };
          }
        );
    in
    {
      overlays.default =
        final: prev:
        let
          jdk = prev."jdk${toString javaVersion}";
        in
        {
          inherit jdk;
          maven = prev.maven.override { jdk_headless = jdk; };
          gradle = prev.gradle.override { java = jdk; };
          lombok = prev.lombok.override { inherit jdk; };
        };

      devShells = forEachSupportedSystem (
        { pkgs }:
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              gcc
              gradle
              jdk
              maven
              ncurses
              patchelf
              zlib
              netbeans
            ];

            shellHook =
              let
                loadLombok = "-javaagent:${pkgs.lombok}/share/java/lombok.jar";
                prev = "\${JAVA_TOOL_OPTIONS:+ $JAVA_TOOL_OPTIONS}";
              in
              ''
                export JAVA_TOOL_OPTIONS="${loadLombok}${prev}"
              '';
          };
        }
      );
    };
}
