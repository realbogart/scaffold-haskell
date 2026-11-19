{
  description = "Scaffold a Haskell project, just the way I like it";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        runtimeInputs = with pkgs; [ coreutils findutils mustache-go ];
        scaffolder = pkgs.writeShellApplication {
          name = "scaffold-haskell";
          inherit runtimeInputs;
          text = ''
            export TEMPLATE_DIR=${./templates}
          '' + builtins.readFile ./scripts/render-project.sh;
        };
      in {
        devShells.default = pkgs.mkShell {
          packages = runtimeInputs ++ [ pkgs.git pkgs.direnv ];
        };

        packages.default = scaffolder;
        apps.default = flake-utils.lib.mkApp { drv = scaffolder; };

        checks.render = pkgs.runCommand "check-scaffold-haskell" {
          nativeBuildInputs = [ scaffolder pkgs.git ];
        } ''
          bash ${./scripts/check-render.sh}
          touch "$out"
        '';
      });
}
