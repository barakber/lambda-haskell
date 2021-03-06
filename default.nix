# Little snippet from Niv to get our pinned dependencies
{ sources ? import ./nix/sources.nix }:
with { overlay = _: pkgs: { niv = import sources.niv {}; }; };

let
    cabal2nixOverlay = self: super: {
      haskellPackages = super.haskellPackages.override (old: {
        overrides = pkgs.lib.composeExtensions old.overrides (self: hspkgs: {
          tls = pkgs.haskell.lib.dontCheck hspkgs.tls;
        });
      });
    };

    # We use a nixpkgs version with modified configuration to build static
    # executables. From https://github.com/nh2/static-haskell-nix
    pkgs = (import (sources.nixpkgs-static + "/survey/default.nix") {
      # This is the nixpkgs revision we are modifying.
      # We pinned the revision with 'niv add NixOS/nixpkgs -a rev=<sha256>
      normalPkgs = import sources.nixpkgs {};
      approach = "pkgsMusl";
      integer-simple = true;
    }).pkgs.pkgsMusl.appendOverlays [cabal2nixOverlay];

    # Nix usually takes all files in the source directory into consideration and
    # busts the package cache whenever any of these files change. To avoid that
    # we use hercules-ci/gitignore.nix so that nix will ignore files included in
    # our .gitignore
    gitignoreSource = (import sources.gitignore { lib = pkgs.lib; }).gitignoreSource;

    lambda-test =
        pkgs.haskell.lib.overrideCabal
            # Create a nix derivation from the cabal package
            (pkgs.haskellPackages.callCabal2nix "lambda-test" (gitignoreSource ./.) {})
            # Set flags that make sure that we are building a truly static executable
            (old: {
                  configureFlags = [
                    "-flambda"
                    "--ghc-option=-optl=-static"
                    "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
                    "--extra-lib-dirs=${pkgs.zlib.static}/lib"
                    "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
                    "--disable-executable-stripping"
                  ];
            });

    # Create a zip file ready to be sent to AWS lambda
    function-zip = pkgs.runCommandNoCC "lambda-test.zip" { buildInputs = [ pkgs.zip ]; }
      ''
          mkdir -p $out
          cp ${lambda-test}/bin/lambda-test bootstrap
          zip $out/function.zip bootstrap
      '';

    # Create a Docker image (use local nixpkgs)
    pkgs' = import <nixpkgs> {};
    docker-image = pkgs'.dockerTools.buildImage {
      name = "lambda-test-docker";
      tag  = "latest";
      contents = [lambda-test];
      config = {
        Cmd = [ "${lambda-test}/bin/lambda-test" ];
      };
    };
in
{ inherit
    lambda-test
    function-zip
    docker-image ;
}
