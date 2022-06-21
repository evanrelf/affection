let
  pkgs = import <nixpkgs> { };

in
pkgs.mkShell {
  buildInputs = with pkgs; [
    cabal-install
    ghc
    ghcid
  ];
}
