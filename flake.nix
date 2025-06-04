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
        templateDir = ./templates;
      in {
        devShells.default = pkgs.mkShell {
          packages = [ pkgs.jq pkgs.mustache-go pkgs.git pkgs.direnv ];
        };

        packages.default = pkgs.writeShellApplication {
          name = "scaffold-haskell";
          runtimeInputs = [ pkgs.jq pkgs.mustache-go ];
          text = ''
            #!/usr/bin/env bash
            export TEMPLATE_DIR=${templateDir}
            exec ${./scripts/render-project.sh} "$@"
          '';
        };

        apps.default =
          flake-utils.lib.mkApp { drv = self.packages.${system}.default; };
      });
}
