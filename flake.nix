{
  # Worth trying in the future: nix flake init -t github:tweag/opam-nix#executable
  description = "A nix flake for twenty_forty_eight";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    }:

    flake-utils.lib.eachDefaultSystem
      (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            opam
            gmp
            pkg-config
            openssl_legacy
            zlib
            libffi
            pcre
            zstd
          ];

          buildInputs = pkgs.lib.optionals pkgs.stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
            CoreServices
          ]);

          shellHook = ''
            opam --version
          '';
        };
      }
      );
}
