let
  pkgs = import ./nix/nixpkgs.nix {};

  affection = import ./default.nix;

in
  affection.env.overrideAttrs (old: {
    buildInputs = with pkgs; old.buildInputs ++ [
      cabal-install
      ghcid
    ];
  })
