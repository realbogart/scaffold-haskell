{
  description = "Scaffold a Haskell project, just the way I like it";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShells.default =
          pkgs.mkShell { packages = [ pkgs.jq pkgs.mustache-go ]; };

        packages.default = pkgs.writeShellApplication {
          name = "scaffold-haskell";
          runtimeInputs = [ pkgs.jq pkgs.mustache-go ];
          text = builtins.readFile ./scripts/render-project.sh;
        };

        apps.default =
          flake-utils.lib.mkApp { drv = self.packages.${system}.default; };
      });
}
