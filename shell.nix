{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, binary, bytestring, Cabal, containers
      , data-default, derive, directory, extensible-exceptions, filepath
      , json, lazysmallcheck2012, mtl, network, old-locale, old-time
      , pandoc, pandoc-types, parsec, process, QuickCheck, random, stdenv
      , syb, tagged, tasty, tasty-quickcheck, text, time
      }:
      mkDerivation {
        pname = "panhandle";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base binary bytestring Cabal containers data-default directory
          extensible-exceptions filepath json mtl network old-locale old-time
          pandoc pandoc-types parsec process random syb text time
        ];
        testHaskellDepends = [
          base binary bytestring Cabal containers data-default derive
          directory extensible-exceptions filepath json lazysmallcheck2012
          mtl network old-locale old-time pandoc pandoc-types parsec process
          QuickCheck random syb tagged tasty tasty-quickcheck text time
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
