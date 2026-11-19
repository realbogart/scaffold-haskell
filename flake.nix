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
          nativeBuildInputs = [ scaffolder ];
        } ''
          scaffold-haskell "with spaces/cool-app"
          cd "with spaces/cool-app"
          test -f .envrc
          test -x ghcid.sh
          test -x scripts/test.sh
          grep -Fx 'module CoolApp where' lib/CoolApp.hs
          grep -F 'exposed-modules:  CoolApp' cool-app.cabal
          grep -F 'CoolAppSpec' test/CoolAppSpec.hs
          if grep -rF '{{' .; then exit 1; fi

          echo 'keep me' > README.md
          cd ../..
          if scaffold-haskell "with spaces/cool-app"; then exit 1; fi
          grep -Fx 'keep me' "with spaces/cool-app/README.md"
          if scaffold-haskell app-123; then exit 1; fi
          test ! -e app-123
          touch "$out"
        '';
      });
}
