with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, binary, bytestring, Cabal, containers
             , data-default, directory, extensible-exceptions, filepath, mtl
             , network, old-locale, old-time, pandoc, pandoc-types, parsec
             , process, random, stdenv, text, time
             }:
             mkDerivation {
               pname = "PanHandle";
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
             }) {};
in
  pkg.env
