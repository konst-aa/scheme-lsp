{
  description = "Tetris written in chicken scheme!";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      defaultBuildInputs =
        let
          stdenv = pkgs.stdenv;
          eggs = import ./eggs.nix { inherit pkgs stdenv; };
        in
        with eggs; [
          pkgs.chicken
          pkgs.gnumake
          args
          json
          uuid
        ];
    in
    {
      defaultPackage.x86_64-linux =
        pkgs.stdenv.mkDerivation {
          src = ./.;

          name = "scheme-lsp";
          buildInputs = defaultBuildInputs;
          buildPhase = ''
          '';
          installPhase = ''
            mkdir -p $out/bin
            cp out $out/bin/chicken-tetris

             for f in $out/bin/*
             do 
               wrapProgram $f \
                --set CHICKEN_REPOSITORY_PATH $CHICKEN_REPOSITORY_PATH
             done
          '';
        }
      ;
      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs =
          defaultBuildInputs ++ [
            pkgs.egg2nix
          ];
      };
    };
}
