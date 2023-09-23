{ nixpkgs ? import (fetchTarball {
  name = "nixpkgs2305";
  url = "https://github.com/NixOS/nixpkgs/archive/23.05.tar.gz";
  sha256 = "10wn0l08j9lgqcw8177nh2ljrnxdrpri7bp0g7nvrsn9rkawvlbf";
}) {
  config = { };
  overlays = [ ];
}, haskellPackages ? nixpkgs.haskellPackages }:
with rec {
  # Uses import-from-derivation to load the function outputted by cabal2nix,
  # and calls it with arguments (dependencies) from haskellPackages.
  rawDrv = haskellPackages.callCabal2nix "panhandle" ./. { };

  # This patches the dependencies of rawDrv to include libopcodes
  fixed = rawDrv.overrideAttrs
    (old: { buildInputs = old.buildInputs ++ [ nixpkgs.libopcodes ]; }) // {
      # Include these in the result, to make overriding easier
      inherit haskellPackages nixpkgs rawDrv;
    };
};
fixed
