let
  pkgs = import ./nix/nixpkgs.nix {};

  effect = import ./default.nix;

in
  effect.env.overrideAttrs (old: {
    buildInputs = with pkgs; old.buildInputs ++ [
      cabal-install
      ghcid
    ];
  })
