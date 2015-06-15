{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, binary, bytestring, Cabal, containers
      , data-default, directory, extensible-exceptions, filepath, mtl
      , network, old-locale, old-time, pandoc, pandoc-types, parsec
      , process, random, stdenv, text, time
      }:
      mkDerivation {
        pname = "panhandle";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          base binary bytestring Cabal containers data-default directory
          extensible-exceptions filepath mtl network old-locale old-time
          pandoc pandoc-types parsec process random text time
        ];
        homepage = "http://chriswarbo.net/essays/activecode";
        description = "Pandoc filter to unwrap nested blocks";
        license = stdenv.lib.licenses.publicDomain;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
