# THIS IS UNNECESSARY FOR BUILDING.
# This is just for my package manager. Ignore if you don't use nix.
{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal cabalInstall
    # Haskell dependencies here
    uuParsinglib optparseApplicative;


in cabal.mkDerivation (self: {
  pname = "mooninite";
  version = "0.0.1.0";
  src = ./.;
  buildDepends = [
    # As imported above
    uuParsinglib optparseApplicative
  ];
  buildTools = [ cabalInstall ];
  enableSplitObjs = false;
})
