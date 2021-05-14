let
  haskellPackagesOverlay = pkgsFinal: pkgsPrev:
    import ./nix/override-haskell-packages.nix
      {
        packages = {
          "affection" = pkgsPrev.nix-gitignore.gitignoreSource [ ] ./.;
        };
        hackage = {
          rev = "52415450270fb5d146097c36e74d1117ba0e4fe4";
          sha256 = "0cc7ls5awhb83jfm8kcaskglgqala32q5s8j87frz0f6wx57gbl4";
        };
      }
      pkgsFinal
      pkgsPrev;


  pkgs = import ./nix/nixpkgs.nix { overlays = [ haskellPackagesOverlay ]; };

in
pkgs.haskellPackages.affection
