{
  description = "Abstract Machines";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };
  outputs =
    { nixpkgs, ... }:
    let
      system = "x86_64-linux";
    in
    {
      devShells.${system}.default =
        let
          pkgs = import nixpkgs { inherit system; };
        in
        pkgs.mkShell {
          packages = with pkgs; [
            # Required packages to build Bonsai? Just comment out the
            # web build in `./bin/dune` if async_ssl fails to build
            gmp pkg-config openssl_legacy zlib
            libffi pcre zstd

            opam

            (pkgs.python3.withPackages (pkgs: with pkgs; [
              matplotlib
              numpy
              networkx
            ]))

            # For visualization
            xdot graphviz feh
          ];

          shellHook = ''
            # Shorter bash prompt, comment out if not wanted
            PS1="\[\e[1;32m\]\W\[\e[0m\]\$ "
          '';
        };
    };
}
