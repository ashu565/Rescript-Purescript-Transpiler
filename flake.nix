{
  description = "Rescript Parsetree";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        shell = pkgs.mkShell {
          buildInputs = with pkgs; [

            git
            coreutils
            curl

            # Haskell packages
            haskell.compiler.ghc98
            cabal-install

            # OCaml packages
            ocaml
            dune_3
            opam
          ];

          # Shell hook to print message when entering the environment
          shellHook = ''
            PS1='\[\033[0;32m\]$(pwd)\[\033[0m\] (Nix Shell) \n\[\033[0;32m\]\$\[\033[0m\] '
            export TERM="xterm-256color"
          '';
        };
      in
        {
          devShells.default = shell;
        }
    );
}