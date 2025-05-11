{
  description = "{{description}}";

  inputs = {
    nixpkgs.url =
      "github:nixos/nixpkgs/041c867bad68dfe34b78b2813028a2e2ea70a23c";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            {{project_name}} = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc9101";
              shell.tools = {
                cabal = { };
                hlint = { };
                haskell-language-server = { };
              };
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
                ghcid
                ormolu
                pkg-config
              ];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.{{project_name}}.flake { };
      in flake // {
        packages = flake.packages // {
          default = flake.packages."{{project_name}}:exe:{{project_name}}";
          container = pkgs.dockerTools.buildLayeredImage {
            name = "{{project_name}}";
            # tag = self.rev or "dirty";
            tag = "latest"; # TODO: Better tagging
            contents = [ flake.packages."{{project_name}}:exe:{{project_name}}" ./web ];
            config = {
              Cmd = [ ];
              Entrypoint = [ "/bin/{{project_name}}" ];
            };
          };
        };
      });
}
