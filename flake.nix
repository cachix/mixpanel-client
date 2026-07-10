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
          testTargets = [ "mixpanel-test" ];
        }) self'.packages.mixpanel-client;

        # Build and run all tests, including the README test which makes live
        # requests to the Mixpanel API. Requires network access, so use
        # `--no-sandbox`. The build sandbox has no CA bundle, so point the TLS
        # stack at `cacert`, otherwise the HTTPS handshake fails with
        # "certificate has unknown CA".
        packages.testAll = pkgs.haskell.lib.compose.overrideCabal (drv: {
          testToolDepends = (drv.testToolDepends or []) ++ [ pkgs.cacert ];
          preCheck = (drv.preCheck or "") + ''
            export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
            export NIX_SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
          '';
        }) config.haskellProjects.default.outputs.packages.mixpanel-client.package;
      };
    };
}
