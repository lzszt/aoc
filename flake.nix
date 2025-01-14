{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    inputs@{ self, ... }:
    let
      pkgs = import ./nix/pkgs.nix {
        inherit inputs;
        system = "x86_64-linux";
      };
      packageName = "AdventOfCode";
    in
    {

      packages.x86_64-linux.${packageName} = pkgs.haskellPackages.AdventOfCode;
      packages.x86_64-linux.default =
        pkgs.haskell.lib.justStaticExecutables
          self.packages.x86_64-linux.${packageName};
      devShells.x86_64-linux = {
        default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.AdventOfCode ];
          nativeBuildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.ghc
            haskellPackages.hlint
            haskellPackages.ghcid
            haskellPackages.haskell-language-server
            haskellPackages.fourmolu
            haskellPackages.cabal-fmt
          ];
        };
      };
    };
}
