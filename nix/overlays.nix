{ inputs }:
let
  customHaskellPackages = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper:
        let
          dontCheck = super.haskell.lib.dontCheck;
          dontHaddock = super.haskell.lib.dontHaddock;

          AdventOfCode-src = self.nix-gitignore.gitignoreSource [
            "*.git"
            "dist"
            "dist-newstyle"
          ] ../.;
          AdventOfCode =
            dontHaddock (hself.callCabal2nix "AdventOfCode" AdventOfCode-src { });
        in {
          # We add ourselves to the set of haskellPackages.
          inherit AdventOfCode;
        };
    };
  };
in [ customHaskellPackages ]
