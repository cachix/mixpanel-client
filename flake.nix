{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, config, ... }: {

        haskellProjects.default = {
          devShell.enable = true;
        };

        # Skip the readme test by default since it requires network access.
        packages.default = pkgs.haskell.lib.compose.overrideCabal (drv: {
          testTarget = "mixpanel-test";
        }) self'.packages.mixpanel-client;

        # Build and run all tests. Requires network access. Use `--no-sandbox`.
        packages.testAll =
          config.haskellProjects.default.outputs.packages.mixpanel-client.package;
      };
    };
}
