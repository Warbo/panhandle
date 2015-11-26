{ mkDerivation, aeson, base, binary, bytestring, Cabal, containers
, data-default, derive, directory, extensible-exceptions, filepath
, lazysmallcheck2012, mtl, network, old-locale, old-time, pandoc
, pandoc-types, parsec, process, QuickCheck, random, stdenv, syb
, tagged, tasty, tasty-quickcheck, text, time
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
    pandoc pandoc-types parsec process random syb text time
  ];
  testHaskellDepends = [
    aeson base binary bytestring Cabal containers data-default derive
    directory extensible-exceptions filepath lazysmallcheck2012 mtl
    network old-locale old-time pandoc pandoc-types parsec process
    QuickCheck random syb tagged tasty tasty-quickcheck text time
  ];
  homepage = "http://chriswarbo.net/essays/activecode";
  description = "Pandoc filter to unwrap nested blocks";
  license = stdenv.lib.licenses.publicDomain;
}
