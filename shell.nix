{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

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
        executableHaskellDepends = [
          base binary bytestring Cabal containers data-default directory
          extensible-exceptions filepath mtl network old-locale old-time
          pandoc pandoc-types parsec process random text time
        ];
        homepage = "http://chriswarbo.net/essays/activecode";
        description = "Pandoc filter to unwrap nested blocks";
        license = stdenv.lib.licenses.publicDomain;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
