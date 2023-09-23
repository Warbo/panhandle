{ nixpkgs ? import (fetchTarball {
  name = "nixpkgs2305";
  url = "https://github.com/NixOS/nixpkgs/archive/23.05.tar.gz";
  sha256 = "10wn0l08j9lgqcw8177nh2ljrnxdrpri7bp0g7nvrsn9rkawvlbf";
}) {
  config = { };
  overlays = [ ];
}, haskellPackages ? nixpkgs.haskellPackages, lib ? nixpkgs.lib }:
with rec {
  inherit (lib) functionArgs setFunctionArgs;

  # Runs cabal2nix on this project's .cabal file. The resulting derivation
  # outputs a default.nix file defining a function, which itself defines a
  # derivation that builds this project.
  panhandle-nix = haskellPackages.haskellSrc2nix {
    name = "panhandle";
    src = lib.cleanSource ./.;
  };

  # Uses import-from-derivation to load the function outputted by panhandle-nix,
  # and calls it with arguments (dependencies) from haskellPackages.
  rawDrv = haskellPackages.callPackage panhandle-nix { };

  # This patches the dependencies of rawDrv to include libopcodes
  fixed = rawDrv.overrideAttrs
    (old: { buildInputs = old.buildInputs ++ [ nixpkgs.libopcodes ]; }) // {
      # Include these in the result, to make overriding easier
      inherit haskellPackages lib nixpkgs panhandle-nix rawDrv;
    };
};
fixed
