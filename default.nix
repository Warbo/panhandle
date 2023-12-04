{ nixpkgs ? import (fetchTarball {
  name = "nixpkgs2311";
  url = "https://github.com/NixOS/nixpkgs/archive/"
    + "057f9aecfb71c4437d2b27d3323df7f93c010b7e.tar.gz";
  sha256 = "1ndiv385w1qyb3b18vw13991fzb9wg4cl21wglk89grsfsnra41k";
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
