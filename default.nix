{ mkDerivation, base, binary, bytestring, Cabal, containers
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
}
