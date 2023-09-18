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

  # Raw cabal2nix result (a function returning a derivation)
  raw = haskellPackages.callCabal2nix "panhandle" (lib.cleanSource ./.);

  # Wrapper around raw, which patches the resulting derivation
  fixed = (args:
    (raw args).overrideAttrs
    (old: { buildInputs = old.buildInputs ++ [ nixpkgs.libopcodes ]; }) // {
      # Include these in the result, in case we want to override
      inherit build raw;
    });

  # Same as fixed, but inherits the function metadata of raw
  build = setFunctionArgs fixed raw;
};
build { }
